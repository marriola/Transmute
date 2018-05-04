namespace TransmuteLib

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

    static member make name isFinal =
        { Name = name;
          IsTarget = false;
          IsFinal = isFinal
        }

    override this.ToString() =
        if this.IsFinal then
            sprintf "(%s)" this.Name
        else
            sprintf "%s" this.Name

type RuleGeneratorState =
    | HasNext of (bool -> State) seq * Transition<State> list * State
    | Done

module SoundChangeRule =
    open System
    open Node
    open System.Collections.Generic

    /// Returns a copy of the state as a final state.
    let makeFinal state = { state with IsFinal = true }

    let private START = State.make "S" false

    let private buildNFA features sets target result environment =
        let getNextState states isInTargetSection =
            let getNext = Seq.head states
            Seq.tail states, getNext isInTargetSection

        /// <param name="featureTrees">The + and - prefix trees for each available feature.</param>
        /// <param name="sets">The prefix trees for each available set.</param>
        /// <param name="states">An infinite sequence of functions that create states.</param>
        /// <param name="input">The list of input symbols (phonemes).</param>
        /// <param name="current">The last node added to the state machine being built.</param>
        /// <param name="transitions">The list of key-value pairs that will create the transition table.</param>
        /// <param name="isAtBeginning">True if none of <c>input</c> has been processed yet; otherwise, false.</param>
        /// <param name="isSubtreeFinal">True if the last state in the subtree should be final.</param>
        /// <param name="isInTargetSection">True if the subtree being built is the target section of the rule; otherwise, false.</param>
        let rec inner
            (featureTrees: IDictionary<string, PrefixTree * PrefixTree>)
            (setTrees: IDictionary<string, PrefixTree>)
            states
            (input: Node list)
            (current: State)
            transitions
            isAtBeginning isSubtreeFinal isInTargetSection =

            let makeFinal state = { state with IsFinal = true }

            let endCata fEnd fNotEnd =
                match input.Tail with
                | [] -> fEnd()
                | _ -> fNotEnd()

            let atEnd() = endCata (fun _ -> true) (fun _ -> false)

            /// Creates a state that matches an input symbol.
            let matchCharacter c =
                let states, target = getNextState states isInTargetSection
                let target = endCata (fun _ -> makeFinal target) (fun _ -> target)
                let transitions = (on c current, [target]) :: transitions
                states, transitions, target

            let transformUtterance utterance =
                let rec innerTransformUtterance utterance states transitions next =
                    match utterance with
                    | [] ->
                        states, transitions, next
                    | c::xs ->
                        let states, target = getNextState states isInTargetSection
                        innerTransformUtterance xs states ((on c next, [target]) :: transitions) target

                innerTransformUtterance (List.ofSeq utterance) states transitions current

            let transformSet setId =
                let states, lastState = getNextState states isInTargetSection
                let rec innerTransformSet (states: (bool -> State) seq) transitions (curState: State) tree =
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
                                        let states, nextState = getNextState states isInTargetSection
                                        let transitions = ((curState, Match.Char c), [nextState]) :: transitions
                                        innerTransformSet states transitions nextState n :: acc
                                    | PrefixTree.Leaf _ ->
                                        let transitions = ((curState, Epsilon), [lastState]) :: transitions
                                        (states, transitions, lastState) :: acc
                                    | Root _ ->
                                        failwith "Root should never have another Root as a descendant"
                                ) [ states, transitions, curState ]
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
                    HasNext 
                        (if isAtBeginning
                            then matchCharacter Special.START
                            else matchCharacter Special.END)
                | TaggedNode (_, UtteranceNode utterance)::_ ->
                    HasNext (transformUtterance utterance)
                | TaggedNode (_, setId)::_ ->
                    HasNext (transformSet setId)
                | _ ->
                    failwithf "Unexpected '%s'" (string input.Head)

            match generatorState with
            | Done ->
                transitions, current
            | HasNext (states, transitions, theNext) ->
                inner featureTrees setTrees states input.Tail theNext transitions false isSubtreeFinal false

        let initialStates = Seq.initInfinite (fun i -> "q" + (string i) |> State.make)

        let featureTrees =
            features
            |> Seq.map (fun (kvp: KeyValuePair<string, Node>) -> kvp.Key, (PrefixTree.fromFeature kvp.Value true, PrefixTree.fromFeature kvp.Value false))
            |> dict

        let setTrees =
            sets
            |> Seq.map (fun (kvp: KeyValuePair<string, Node>) -> kvp.Key, PrefixTree.fromSet kvp.Value)
            |> dict

        let ts, _ = inner featureTrees setTrees initialStates environment START List.empty true true false
        List.rev ts |> groupTransitions

    let createStateMachine features sets rule =
        match untag rule with
        | RuleNode (target, result, environment) ->
            buildNFA features sets target result environment
        | _ ->
            raise (ArgumentException("Must be a RuleNode", "rule"))
