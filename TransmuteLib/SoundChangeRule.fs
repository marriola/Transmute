namespace TransmuteLib

open System
open Node
open StateMachine

module SoundChangeRule =
    open System.Text

    type private Special =
        static member START = '␂'
        static member END = '␃'
        static member JOINER = '\u200d'

    type State =
        | State of name:string * isTarget:bool * isFinal:bool
        | MergedState of State list
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
                    |> Seq.collect (function
                        | State _ as state -> [ state ]
                        | MergedState states -> states)
                    |> Seq.distinct
                    |> Seq.sortBy State.ord
                    |> List.ofSeq
                match statesToMerge with
                | [state] -> state
                | _ -> MergedState statesToMerge

            static member isFinal = function
                | State (_, _, isFinal) -> isFinal
                | MergedState states ->
                    List.exists State.isFinal states

            static member isTarget = function
                | State (_, isTarget, _) -> isTarget
                | MergedState states ->
                    List.exists State.isTarget states

            static member name = function
                | State (name, _, _) -> name
                | MergedState states ->
                    states
                    |> Seq.map State.name
                    |> String.concat (string Special.JOINER)

            static member ord = function
                | State (name, _, _) ->
                    match name.[1..] with
                    | "" -> -1
                    | x -> int x
                | MergedState _ ->
                    failwith "Merged states have no ordinal"

            override this.ToString() =
                if State.isFinal this
                    then State.name this |> sprintf "(%s)"
                    else State.name this |> sprintf "%s"

    type private RuleGeneratorState =
        | HasNext of State seq * Transition<State> list * State
        | Done

    let private START = State.make "S"
    let private ERROR = State.make "Error"

    /// Returns true if both states are equal, or if one state is a merged state that contains the other.
    let inline private (<%>) x y =
        match x, y with
        | (State _ as a), (State _ as b)
        | (MergedState _ as a), (MergedState _ as b) when a = b -> true
        | (State _ as s), MergedState children
        | MergedState children, (State _ as s) when List.contains s children -> true
        | _ -> false

    let private (|IsOrContains|_|) x y =
        if x <%> y
            then Some x
            else None

    let private getFrom ((from, _), _) = from
    let private getInput ((_, on), _) = on
    let private getDest (_, ``to``) = ``to``

    let private hasEpsilonTransitions transitions state =
        transitions
        |> List.exists (fun ((from, c), _) -> c = Epsilon && state <%> from)

    let private transitionsFrom transitions state =
        transitions
        |> List.choose (fun ((from, on), ``to``) ->
            match state with
            | IsOrContains from _ ->
                Some ((state, on), ``to``)
            | _ -> None)

    let private replaceOrigin state ((from, on), ``to`` as t) =
        if from <%> state
            then (state, on), ``to``
            else t
        
    let private replaceDest state ((from, on), ``to`` as t) =
        if ``to`` <%> state
            then (from, on), state
            else t

    let private hasTransition fInputSymbol transitions state =
        transitions
        |> List.exists (fun ((from, on), _) -> on |> fInputSymbol && from = state)

    let private hasEpsilonTransition transitions state = hasTransition ((=) Epsilon) transitions state
    let private hasNonEpsilonTransition transitions state = hasTransition ((<>) Epsilon) transitions state

    let private computeFollowSet transitions state =
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

    let private convertToDfa (table: Transition<State> list) =
        let followTransitions origin transitions =
            // For each transition, get the states that can be reached from its destination by
            // non-epsilon transitions, and create transitions to them from the given state.
            transitions
            |> List.collect (getDest >> computeFollowSet table)
            |> List.distinct
            |> List.map (fun (input, dest) -> (origin, input), dest)

        /// Recursively follows the destination of each transition to a state that has non-epsilon
        /// transitions, eliminating any that have only epsilon transitions.
        let followDestination transitions =
            let rec inner acc transitions =
                match transitions with
                | [] -> acc |> List.ofSeq
                | _ ->
                    // Cross each transition with each state that can be reached from its destination by a non-epsilon transition.
                    // Keep the original transition as well if its destination also has one or more non-epsilon transitions.
                    let followTransitions =
                        transitions
                        |> List.collect (fun ((origin, input), dest as t) ->
                            // Return all epsilon transitions from states that include the destination, plus the
                            // original transition if the destination has non-epsilon transitions or is final.
                            let originalTransition =
                                if State.isFinal dest || hasNonEpsilonTransition table dest
                                    then [t]
                                    else []
                            let followedTransitions =
                                table
                                |> List.choose (function
                                    | (from, Epsilon), follow when from <%> dest ->
                                        Some ((origin, input), follow)
                                    | _ -> None)
                            originalTransition @ followedTransitions)
                        |> List.distinct
                    // Follow transitions to states with epsilon transitions that we haven't already been to
                    let nextTransitions =
                        followTransitions
                        |> List.where (fun ((_, follow) as t) -> not <| List.contains t transitions && hasEpsilonTransition table follow)
                    // Accumulate states 
                    let nextAcc =
                        followTransitions
                        |> List.where (fun (_, follow) -> State.isFinal follow || hasNonEpsilonTransition table follow)
                        |> Set.ofList
                        |> Set.union acc
                    inner nextAcc nextTransitions

            inner (Set.empty) transitions

        let rec inner stack dfaTransitions =
            match stack with
            | [] -> dfaTransitions |> List.ofSeq
            | current::stack ->
                // Separate epsilon and non-epsilon transitions
                let epsilonTransitions, nonEpsilonTransitions =
                    transitionsFrom table current
                    |> List.partition (fun ((_, c), _) -> c = Epsilon)
                // Follow epsilon transitions to the next state with non-epsilon transitions
                let followedEpsilonTransitions = followTransitions current epsilonTransitions
                // Combine with followed epsilon transitions, and follow the destination state if it has epsilon transitions.
                let transitionsFromCurrent =
                    nonEpsilonTransitions @ followedEpsilonTransitions |> followDestination
                // Group all transitions by input symbol
                let groupedTransitions =
                    transitionsFromCurrent
                    |> List.distinct
                    |> List.groupBy getInput
                // Merge states that can be reached by the same input symbol
                let mergedTransitions =
                    groupedTransitions
                    |> List.where (fun (_, dests) -> List.length dests >= 2)
                    |> List.map (fun (on, dests) ->
                        let mergedState =
                            dests
                            |> List.map getDest
                            |> List.distinct
                            |> State.merge
                        (current, on), mergedState)
                // All other transitions
                let singleTransitions =
                    groupedTransitions
                    |> List.where (getDest >> List.length >> (=) 1)
                    |> List.collect snd
                let allTransitions =
                    mergedTransitions @ singleTransitions
                    |> List.distinct
                // Follow transitions that don't go to the current state or a state already in the stack
                let nextStack =
                    allTransitions
                    |> List.map getDest
                    |> List.where (fun s -> s <> current && not (List.contains s stack))
                    |> List.append stack
                    |> List.distinct
                let nextTransitions =
                    allTransitions
                    |> Set.ofList
                    |> Set.union dfaTransitions
                inner nextStack nextTransitions

        printf "NFA:\n\n"
        table
        |> List.indexed
        |> List.map (fun (i, ((fromState, m), toState)) -> sprintf "%d.\t(%s, %s)\t-> %s" i (string fromState) (string m) (string toState))
        |> String.concat "\n"
        |> Console.WriteLine

        inner [START] (Set.empty)

    let private buildStateMachine features sets target result environment =
        let takeState (states: State seq) =
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
                        (fun _ -> State.makeFinal nextState)
                        (fun _ -> nextState)
                states, nextState

            /// Creates a transition to a new state that matches an input symbol.
            let matchCharacter c =
                let states, target = getNextState states
                let transitions = (on c current, target) :: transitions
                states, transitions, target

            /// Creates a series of states and transitions that match each character of an utterance.
            let transformUtterance utterance =
                let rec innerTransformUtterance utterance states transitions next =
                    match utterance with
                    | [] ->
                        states, transitions, next
                    | c::xs ->
                        let states, target = getNextState states
                        innerTransformUtterance xs states ((on c next, target) :: transitions) target

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
                        let states, transitions =
                            children
                            |> List.fold
                                (fun (states, transitions) n ->
                                    match n with
                                    | PrefixTree.Node (_, c, _) ->
                                        let states, nextState = takeState states
                                        let transitions = ((curState, Match.Char c), nextState) :: transitions
                                        let states, transitions, _ = innerTransformSet states transitions nextState n
                                        states, transitions
                                    | PrefixTree.Leaf _ ->
                                        let transitions = ((curState, Epsilon), lastState) :: transitions
                                        states, transitions
                                    | Root _ ->
                                        failwith "A Root should never have another Root as a descendant")
                                (states, transitions)
                        states, transitions, lastState
                    | PrefixTree.Leaf _ ->
                        states, transitions, curState

                PrefixTree.fromSetIntersection features sets setId
                |> innerTransformSet states transitions current

            let transformOptional children =
                let states, lastState = getNextState states
                let states, transitions, subtreeLast = inner states children current transitions true false
                let transitions =
                    [ (current, Epsilon), lastState
                      (subtreeLast, Epsilon), lastState ] @
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
                    |> List.rev
                    |> List.tail
                    |> List.map (fun (_, _, subtreeFinal) -> (subtreeFinal, Epsilon), lastState)
                let transitions =
                    [ (current, Any), ERROR ] @ // Go to ERROR if we can't match anything
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
        //|> groupTransitions
        |> convertToDfa
        |> dict

    let compile features sets rule =
        match untag rule with
        | RuleNode (target, result, environment) ->
            buildStateMachine features sets target result environment
        | _ ->
            raise (ArgumentException("Must be a RuleNode", "rule"))

    type Result =
        | Match of result:string
        | Mismatch

    let matchWord rule word =
        stateMachineConfig()
        |> withTransitions rule
        |> withStartState START
        |> withErrorState ERROR
        |> withInitialValue false
        |> onError (fun _ currentValue getNextValue -> currentValue |> getNextValue |> Restart)
        |> onTransition (fun isNextFinal _ _ _ _ value -> isNextFinal || value)
        |> onAccept State.isFinal
        |> onFinish (fun value -> value |> string |> Match)
        |> runStateMachine (sprintf "%c%s%c" Special.START word Special.END)
            
