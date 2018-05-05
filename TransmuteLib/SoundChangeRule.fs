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

            let inline giveTrue () = true
            let inline giveFalse () = false

            let endCata fEnd fNotEnd =
                if not isSubtreeFinal then
                    fNotEnd()
                else
                    match input with
                    | _::[] -> fEnd()
                    | _ -> fNotEnd()

            let atEnd() = endCata giveTrue giveFalse

            /// Creates a transition to a new state that matches an input symbol.
            let matchCharacter c =
                let states, target = getNextState states
                let target = endCata (fun _ -> makeFinal target) (fun _ -> target)
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
                        let target =
                            if isSubtreeFinal
                                then makeFinal target
                                else target
                        innerTransformUtterance xs states ((on c next, [target]) :: transitions) target

                innerTransformUtterance (List.ofSeq utterance) states transitions current

            /// Computes the intersection of a list of feature and set identifiers, and creates a
            /// tree of states and transitions that match each member of the intersection.
            let transformSet setId =
                let states, lastState = getNextState states
                let lastState =
                    endCata
                        (fun _ -> makeFinal lastState)
                        (fun _ -> lastState)

                let rec innerTransformSet states transitions curState tree =
                    match tree with
                    | PrefixTree.Root children
                    | PrefixTree.Node (_, _, children) ->
                        // Create states for each child and transitions to them
                        let follows =
                            List.fold
                                (fun acc n ->
                                    let states, transitions = List.head acc
                                    match n with
                                    | PrefixTree.Node (_, c, _) ->
                                        let states, nextState = getNextState states
                                        let transitions = ((curState, Match.Char c), [nextState]) :: transitions
                                        let x, y, _ = innerTransformSet states transitions nextState n
                                        (x, y) :: acc
                                    | PrefixTree.Leaf _ ->
                                        let transitions = ((curState, Epsilon), [lastState]) :: transitions
                                        (states, transitions) :: acc
                                    | Root _ ->
                                        failwith "A Root should never have another Root as a descendant")
                                [ states, transitions ]
                                children
                        match follows with
                        | (states, transitions)::_ ->
                            states, transitions, lastState
                        | _ ->
                            failwith "children must not be empty"
                    | PrefixTree.Leaf _ ->
                        states, transitions, curState

                PrefixTree.fromSetIntersection features sets setId
                |> innerTransformSet states transitions current

            let generatorState =
                match input with
                | [] ->
                    Done
                | TaggedNode (_, PlaceholderNode)::_ ->
                    HasNext (inner states target current transitions true (atEnd()))
                | TaggedNode (_, BoundaryNode)::_ ->
                    let boundaryChar = if isAtBeginning then Special.START else Special.END
                    HasNext (matchCharacter boundaryChar)
                | TaggedNode (_, UtteranceNode utterance)::_ ->
                    HasNext (transformUtterance utterance)
                | TaggedNode (_, SetIdentifierNode setId)::_ ->
                    HasNext (transformSet setId)
                | TaggedNode (_, (IdentifierNode _ as id))::_ ->
                    HasNext (transformSet [ id ])
                | _ ->
                    failwithf "Unexpected '%s'" (string input.Head)

            match generatorState with
            | Done ->
                states, transitions, current
            | HasNext (states, transitions, theNext) ->
                inner states input.Tail theNext transitions false isSubtreeFinal

        let initialStates = Seq.initInfinite (State.make << sprintf "q%d")
        let _, ts, _ = inner initialStates environment START List.empty true true

        ts |> List.rev |> groupTransitions

    let createStateMachine features sets rule =
        match untag rule with
        | RuleNode (target, result, environment) ->
            buildNFA features sets target result environment
        | _ ->
            raise (ArgumentException("Must be a RuleNode", "rule"))
