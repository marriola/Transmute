namespace TransmuteLib

open System
open Node
open StateMachine

type Special =
    static member START = '␂'
    static member END = '␃'
    static member JOINER = '\u200d'

type RuleState =
    | State of name:string * isTarget:bool * isFinal:bool
    | MergedState of RuleState list
    with
        static member make name = State (name, isTarget=false, isFinal=false)
        
        static member makeFinal = function
            | State (name, isTarget, _) ->
                State (name, isTarget, isFinal=true)
            | MergedState _ as state ->
                failwithf "%s is a merged state; it cannot be made final" (string state)

        static member merge states =
            let statesToMerge =
                states
                |> Seq.map (function
                    | State _ as state -> [ state ]
                    | MergedState states -> states)
                |> Seq.concat
                |> Seq.distinct
                |> Seq.sortBy RuleState.ord
                |> List.ofSeq
            match statesToMerge with
            | [state] -> state
            | _ -> MergedState statesToMerge

        static member isFinal = function
            | State (_, _, isFinal) -> isFinal
            | MergedState states ->
                List.exists RuleState.isFinal states

        static member isTarget = function
            | State (_, isTarget, _) -> isTarget
            | MergedState states ->
                List.exists RuleState.isTarget states

        static member name = function
            | State (name, _, _) -> name
            | MergedState states ->
                states
                |> Seq.map RuleState.name
                |> String.concat (string Special.JOINER)

        static member ord = function
            | State (name, _, _) ->
                match name.[1..] with
                | "" -> -1
                | x -> int x
            | MergedState _ ->
                failwith "Merged states have no ordinal"

        override this.ToString() =
            if RuleState.isFinal this
                then RuleState.name this |> sprintf "(%s)"
                else RuleState.name this |> sprintf "%s"

type RuleGeneratorState =
    | HasNext of RuleState seq * Transition<RuleState> list * RuleState
    | Done

module SoundChangeRule =
    let private START = RuleState.make "S"
    let private ERROR = RuleState.make "Error"

    /// Returns true if both states are equal, or if one state is a merged state that contains the other.
    let inline (<%>) x y =
        match x, y with
        | (State _ as a), (State _ as b)
        | (MergedState _ as a), (MergedState _ as b) when a = b -> true
        | (State _ as s), MergedState children
        | MergedState children, (State _ as s) when List.contains s children -> true
        | _ -> false

    let (|IsOrContains|_|) x y =
        if x <%> y
            then Some x
            else None

    let getFrom ((from, _), _) = from
    let getInput ((_, on), _) = on
    let getDest (_, ``to``) = ``to``

    let hasEpsilonTransitions transitions state =
        transitions
        |> List.exists (fun ((from, c), _) -> c = Epsilon && state <%> from)

    let transitionsFrom transitions state =
        transitions
        |> List.choose (fun ((from, on), ``to``) ->
            match state with
            | IsOrContains from _ ->
                Some ((state, on), ``to``)
            | _ -> None)

    let replaceOrigin state ((from, on), ``to`` as t) =
        if from <%> state
            then (state, on), ``to``
            else t
        
    let replaceDest state ((from, on), ``to`` as t) =
        if ``to`` <%> state
            then (from, on), state
            else t

    let hasTransition fInputSymbol transitions state =
        transitions
        |> List.exists (fun ((from, on), _) -> on |> fInputSymbol && from = state)

    let hasEpsilonTransition transitions state = hasTransition ((=) Epsilon) transitions state
    let hasNonEpsilonTransition transitions state = hasTransition ((<>) Epsilon) transitions state

    let computeFollowSet transitions state =
        let rec inner states result =
            match states with
            | [] ->
                List.rev result
            | x::xs ->
                // States to which we can ε transition from x
                let followStates =
                    transitions
                    |> List.filter (function
                        | (from, Epsilon), _ when from = x -> true
                        | _ -> false)
                    |> List.map (fun (_, ``to``) -> ``to``)
                    |> List.concat
                    |> set
                // Non-ε transitions we can take from those states
                let followTransitions =
                    transitions
                    |> List.filter (fun ((from, input), _) -> input <> Epsilon && (from <%> x || followStates.Contains(from)))
                    |> List.map (fun ((_, input), ``to``) -> input, ``to``)
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

    let private convertToDfa (table: Transition<RuleState> list) =
        let followTransitions origin transitions =
            transitions
            |> List.map (getDest >> List.head >> computeFollowSet table)
            |> List.concat
            |> List.distinct
            |> List.map (fun (input, dest) -> (origin, input), dest)

        /// Recursively follows the destination of each transition to a state that has non-epsilon
        /// transitions, eliminating any that have only epsilon transitions.
        let followDestination transitions =
            let rec inner acc transitions =
                match transitions with
                | [] -> acc |> List.ofSeq
                | _ ->
                    let followTransitions =
                        transitions
                        |> List.map (fun ((origin, input), dest::_ as t) ->
                            let originalTransition =
                                if RuleState.isFinal dest || hasNonEpsilonTransition table dest
                                    then [t]
                                    else []
                            let followedTransitions =
                                table
                                |> List.choose (function
                                    | (from, Epsilon), follow when from <%> dest ->
                                        Some ((origin, input), follow)
                                    | _ -> None)
                            originalTransition @ followedTransitions)
                        |> List.concat
                        |> List.where (fun ((_, follow::_) as t) -> RuleState.isFinal follow || not <| List.contains t transitions)
                        |> List.distinct
                    let nextTransitions =
                        followTransitions
                        |> List.where (fun (_, follow::_) -> hasEpsilonTransition table follow)
                    let nextAcc =
                        transitions @ followTransitions
                        |> List.where (fun (_, follow::_) -> RuleState.isFinal follow || hasNonEpsilonTransition table follow)
                        |> Set.ofList
                        |> Set.union acc
                    inner nextAcc nextTransitions

            inner (Set.empty) transitions

        let rec inner stack dfaTransitions =
            match stack with
            | [] -> dfaTransitions |> List.ofSeq
            | current::stack ->
                // Separate out epsilon and non-epsilon transitions
                let epsilonTransitions, nonEpsilonTransitions =
                    transitionsFrom table current
                    |> List.partition (fun ((_, c), _) -> c = Epsilon)
                // Follow epsilon transitions to next state with non-epsilon transitions
                let followedEpsilonTransitions = followTransitions current epsilonTransitions
                // Combine with followed epsilon transitions, and follow the destination state if it has epsilon transitions.
                let transitionsFromCurrent =
                    nonEpsilonTransitions @ followedEpsilonTransitions |> followDestination
                // Group all transitions by input symbol, merging states that can be reached by the same input symbol.
                let groupedTransitions =
                    transitionsFromCurrent
                    |> List.distinct
                    |> List.groupBy getInput
                let mergedTransitions =
                    groupedTransitions
                    |> List.where (fun (_, dests) -> List.length dests > 1)
                    |> List.map (fun (on, dests) ->
                        let mergedState =
                            dests
                            |> List.map getDest
                            |> List.concat
                            |> List.distinct
                            |> RuleState.merge
                        (current, on), [mergedState])
                let singleTransitions =
                    groupedTransitions
                    |> List.where (getDest >> List.length >> (=) 1)
                    |> List.map getDest
                    |> List.concat
                let transitionsToTake = mergedTransitions @ singleTransitions |> List.distinct
                let nextStack =
                    transitionsToTake
                    |> List.map getDest
                    |> List.concat
                    |> List.where (fun s -> s <> current && not (List.contains s stack))
                    |> List.append stack
                    |> List.distinct
                let nextTransitions =
                    transitionsToTake
                    |> Set.ofList
                    |> Set.union dfaTransitions
                inner nextStack nextTransitions

        table
        |> List.indexed
        |> List.map (fun (i, ((fromState, m), toState)) -> sprintf "%d.\t(%s, %s)\t-> %s" i (string fromState) (string m) (string toState))
        |> String.concat "\n"
        |> Console.WriteLine

        inner [START] (Set.empty)

    let private buildNFA features sets target result environment =
        let takeState (states: RuleState seq) =
            Seq.tail states, Seq.head states

        /// <param name="states">An infinite sequence of functions that create states.</param>
        /// <param name="input">The list of input symbols (phonemes).</param>
        /// <param name="current">The last node added to the state machine being built.</param>
        /// <param name="transitions">The list of key-value pairs that will create the transition table.</param>
        /// <param name="isAtBeginning">True if none of <c>input</c> has been processed yet; otherwise, false.</param>
        /// <param name="isSubtreeFinal">True if the last state in the subtree should be final.</param>
        let rec inner states (input: Node list) current transitions isAtBeginning isSubtreeFinal =
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
                        (fun _ -> RuleState.makeFinal nextState)
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

        let initialStates = Seq.initInfinite (sprintf "q%d" >> RuleState.make)
        let _, ts, _ = inner initialStates environment START List.empty true true
        ts
        |> List.rev
        |> groupTransitions
        |> convertToDfa

    let createStateMachine features sets rule =
        match untag rule with
        | RuleNode (target, result, environment) ->
            buildNFA features sets target result environment
        | _ ->
            raise (ArgumentException("Must be a RuleNode", "rule"))
