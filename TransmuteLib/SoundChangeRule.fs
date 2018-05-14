﻿namespace TransmuteLib

open System
open Node
open StateMachine

type Special =
    static member START = '␂'
    static member END = '␃'

type State =
    { Name: string;
      IsTarget: bool;
      IsFinal: bool
    }

    with

    static member make name =
        { Name = name;
          IsTarget = false;
          IsFinal = false }

    static member merge states =
        { Name = states |> List.map (fun s -> s.Name) |> String.concat "";
          IsTarget = states |> List.exists (fun s -> s.IsTarget);
          IsFinal = states |> List.exists (fun s -> s.IsFinal) }

    override this.ToString() =
        if this.IsFinal then
            sprintf "(%s)" this.Name
        else
            sprintf "%s" this.Name

type RuleGeneratorState =
    | HasNext of State seq * Transition<State> list * State
    | Done

module SoundChangeRule =
    /// Returns a copy of the state as a final state.
    let makeFinal state = { state with IsFinal = true }

    let private START = State.make "S"
    let private ERROR = State.make "Error"

    let computeFollowSet transitions state =
        let rec inner states result =
            match states with
            | [] ->
                List.rev result
            | x::xs ->
                // State to which we can ε transition from x
                let followStates =
                    transitions
                    |> List.filter (function
                        | ((from, Epsilon), _) when from = x -> true
                        | _ -> false)
                    |> List.map (fun (_, ``to``) -> ``to``)
                    |> List.concat
                    |> set
                // Non-ε transitions we can take from those states
                let followTransitions =
                    transitions
                    |> List.filter (fun ((from, input), _) -> input <> Epsilon && followStates.Contains(from))
                    |> List.map (fun ((_, input), ``to``) -> input, ``to``)
                // States to which we can ε transition
                let nextStates =
                    followStates
                    |> Seq.filter (fun s ->
                        transitions
                            |> List.exists (function
                                | (from, Epsilon), _ when from = s -> true
                                | _ -> false))
                    |> List.ofSeq
                inner (nextStates @ xs) (followTransitions @ result)
        inner [state] []

    let private removeNondeterminism (transitions: ((State * Match) * State list) list) =
        transitions

    let private buildNFA features sets target result environment =
        let takeState states =
            Seq.tail states, Seq.head states

        /// <param name="states">An infinite sequence of functions that create states.</param>
        /// <param name="input">The list of input symbols (phonemes).</param>
        /// <param name="current">The last node added to the state machine being built.</param>
        /// <param name="transitions">The list of key-value pairs that will create the transition table.</param>
        /// <param name="isAtBeginning">True if none of <c>input</c> has been processed yet; otherwise, false.</param>
        /// <param name="isSubtreeFinal">True if the last state in the subtree should be final.</param>
        let rec inner states (input: Node list) current transitions isAtBeginning isSubtreeFinal =
            let makeFinal state = { state with IsFinal = true }

            let inline giveTrue () = true
            let inline giveFalse () = false

            let endCata fEnd fNotEnd =
                if not isSubtreeFinal then
                    fNotEnd()
                else
                    match input with
                    | _::[] -> fEnd()
                    | _ -> fNotEnd()

            let isInputAtEnd() = endCata giveTrue giveFalse

            let getNextState states =
                let states, nextState = takeState states
                let nextState =
                    endCata
                        (fun _ -> makeFinal nextState)
                        (fun _ -> nextState)
                states, nextState

            /// Creates a transition to a new state that matches an input symbol.
            let matchCharacter c =
                let states, target = getNextState states
                let transitions = (on c current, [target]) :: transitions
                states, transitions, target

            /// Creates a series of states and transitions that match each character of an utterance.
            let transformUtterance utterance =
                let rec innerTransformUtterance utterance states transitions next =
                    match utterance with
                    | [] ->
                        states, transitions, next
                    | c::xs ->
                        let states, target = getNextState states
                        innerTransformUtterance xs states ((on c next, [target]) :: transitions) target

                innerTransformUtterance (List.ofSeq utterance) states transitions current

            /// Computes the intersection of a list of feature and set identifiers, and creates a
            /// tree of states and transitions that match each member of the intersection.
            let transformSet setId =
                let states, lastState = getNextState states

                let rec innerTransformSet states transitions curState tree =
                    match tree with
                    | PrefixTree.Root children
                    | PrefixTree.Node (_, _, children) ->
                        // Create states for each child and transitions to them
                        let follows =
                            List.fold
                                (fun acc n ->
                                    let states, transitions = acc
                                    match n with
                                    | PrefixTree.Node (_, c, _) ->
                                        let states, nextState = takeState states
                                        let transitions = ((curState, Match.Char c), [nextState]) :: transitions
                                        let states, transitions, _ = innerTransformSet states transitions nextState n
                                        states, transitions
                                    | PrefixTree.Leaf _ ->
                                        let transitions = ((curState, Epsilon), [lastState]) :: transitions
                                        states, transitions
                                    | Root _ ->
                                        failwith "A Root should never have another Root as a descendant")
                                (states, transitions)
                                children
                        let states, transitions = follows
                        states, transitions, lastState
                    | PrefixTree.Leaf _ ->
                        states, transitions, curState

                PrefixTree.fromSetIntersection features sets setId
                |> innerTransformSet states transitions current

            let transformOptional children =
                let states, lastState = getNextState states
                let states, transitions, subtreeLast = inner states children current transitions true false
                let transitions =
                    [ (current, Epsilon), [lastState]
                      (subtreeLast, Epsilon), [lastState] ] @
                    transitions
                states, transitions, lastState

            let transformDisjunct branches =
                let states, lastState = getNextState states
                // Build subtree for each branch
                let follows =
                    List.foldBack
                        (fun branch ((states, transitions, _)::_ as acc) ->
                            inner states branch current transitions true (isSubtreeFinal && isInputAtEnd()) :: acc)
                        branches
                        [ states, transitions, current ]
                let states, transitions, _ = List.head follows
                // Transition from last state of each subtree to lastState.
                // Reverse the list and take the tail first so we don't epsilon from current to lastState.
                let subtreeFinalToLastState =
                    follows
                    |> List.tail
                    |> List.map (fun (_, _, subtreeFinal) -> (subtreeFinal, Epsilon), [lastState])
                let transitions =
                    [ (current, Any), [ERROR] ] @ // Go to ERROR if we can't match anything
                    subtreeFinalToLastState @
                    transitions
                states, transitions, lastState

            let generatorState =
                match input with
                | [] ->
                    Done
                | TaggedNode (_, BoundaryNode)::_ ->
                    let boundaryChar = if isAtBeginning then Special.START else Special.END
                    HasNext (matchCharacter boundaryChar)
                | TaggedNode (_, UtteranceNode utterance)::_ ->
                    HasNext (transformUtterance utterance)
                | TaggedNode (_, SetIdentifierNode setId)::_ ->
                    HasNext (transformSet setId)
                | TaggedNode (_, (IdentifierNode _ as id))::_ ->
                    HasNext (transformSet [ id ])
                | TaggedNode (_, PlaceholderNode)::_ ->
                    HasNext (inner states target current transitions true (isInputAtEnd()))
                | TaggedNode (_, OptionalNode children)::_ ->
                    HasNext (transformOptional children)
                | TaggedNode (_, DisjunctNode branches):: _ ->
                    HasNext (transformDisjunct branches)
                | _ ->
                    failwithf "Unexpected '%s'" (string input.Head)

            match generatorState with
            | Done ->
                states, transitions, current
            | HasNext (states, transitions, theNext) ->
                inner states input.Tail theNext transitions false isSubtreeFinal

        let initialStates = Seq.initInfinite (sprintf "q%d" >> State.make)
        let _, ts, _ = inner initialStates environment START List.empty true true
        ts
        |> List.rev
        |> groupTransitions
        |> removeNondeterminism

    let createStateMachine features sets rule =
        match untag rule with
        | RuleNode (target, result, environment) ->
            buildNFA features sets target result environment
        | _ ->
            raise (ArgumentException("Must be a RuleNode", "rule"))
