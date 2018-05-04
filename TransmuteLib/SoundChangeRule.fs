namespace TransmuteLib

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
          IsFinal = false
        }

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

    let private buildNFA features sets target result environment =
        let getNextState states =
            Seq.tail states, Seq.head states

        /// <param name="states">An infinite sequence of functions that create states.</param>
        /// <param name="input">The list of input symbols (phonemes).</param>
        /// <param name="current">The last node added to the state machine being built.</param>
        /// <param name="transitions">The list of key-value pairs that will create the transition table.</param>
        /// <param name="isAtBeginning">True if none of <c>input</c> has been processed yet; otherwise, false.</param>
        /// <param name="isSubtreeFinal">True if the last state in the subtree should be final.</param>
        let rec inner states (input: Node list) current transitions isAtBeginning isSubtreeFinal =
            let makeFinal state = { state with IsFinal = true }

            let endCata fEnd fNotEnd =
                match input.Tail with
                | [] -> fEnd()
                | _ -> fNotEnd()

            let atEnd() = endCata (fun _ -> true) (fun _ -> false)

            /// Creates a state that matches an input symbol.
            let matchCharacter c =
                let states, target = getNextState states
                let target = endCata (fun _ -> makeFinal target) (fun _ -> target)
                let transitions = (on c current, [target]) :: transitions
                states, transitions, target

            let transformUtterance utterance =
                let rec innerTransformUtterance utterance states transitions next =
                    match utterance with
                    | [] ->
                        states, transitions, next
                    | c::xs ->
                        let states, target = getNextState states
                        innerTransformUtterance xs states ((on c next, [target]) :: transitions) target

                innerTransformUtterance (List.ofSeq utterance) states transitions current

            let transformSet setId =
                let states, lastState = getNextState states
                let rec innerTransformSet states transitions curState tree =
                    match tree with
                    | PrefixTree.Root children
                    | PrefixTree.Node (_, _, children) ->
                        let follows =
                            children
                            |> List.fold
                                (fun acc n ->
                                    let states, transitions, _ = List.head acc
                                    match n with
                                    | PrefixTree.Node (_, c, _) ->
                                        let states, nextState = getNextState states
                                        let transitions = ((curState, Match.Char c), [nextState]) :: transitions
                                        innerTransformSet states transitions nextState n :: acc
                                    | PrefixTree.Leaf _ ->
                                        let transitions = ((curState, Epsilon), [lastState]) :: transitions
                                        (states, transitions, lastState) :: acc
                                    | Root _ ->
                                        failwith "Root should never have another Root as a descendant")
                                [ states, transitions, curState ]
                        let states, transitions, _ = List.head follows
                        let lastState =
                            endCata
                                (fun _ -> makeFinal lastState)
                                (fun _ -> lastState)
                        states, transitions, lastState
                    | PrefixTree.Leaf _ ->
                        states, transitions, curState

                PrefixTree.fromSetIntersection features sets setId
                |> innerTransformSet states transitions current

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
                | _ ->
                    failwithf "Unexpected '%s'" (string input.Head)

            match generatorState with
            | Done ->
                transitions, current
            | HasNext (states, transitions, theNext) ->
                inner states input.Tail theNext transitions false isSubtreeFinal

        let initialStates = Seq.initInfinite (State.make << sprintf "q%d")
        let ts, _ = inner initialStates environment START List.empty true true

        ts |> List.rev |> groupTransitions

    let createStateMachine features sets rule =
        match untag rule with
        | RuleNode (target, result, environment) ->
            buildNFA features sets target result environment
        | _ ->
            raise (ArgumentException("Must be a RuleNode", "rule"))
