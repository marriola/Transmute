namespace TransmuteLib

open System
open System.Collections.Generic
open Node
open StateMachine

module SoundChangeRule =
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

    type Transformation = Transition<State> * string
    type TransformationTable = IDictionary<Transition<State>, string>

    type private RuleGeneratorState =
        | HasNext of State seq * Node list * Transition<State> list * Transformation list * State
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

    let private getOrigin ((from, _), _) = from
    let private getInput ((_, on), _) = on
    let private getDest (_, ``to``) = ``to``

    let private transitionsFrom state transitions =
        transitions
        |> List.choose (fun ((from, on), ``to``) ->
            match state with
            | IsOrContains from _ ->
                Some ((state, on), ``to``)
            | _ -> None)

    let private hasTransition fCompare transitions state =
        transitions
        |> List.exists (fun ((origin, input), _) -> state = origin && fCompare input)

    let private hasEpsilonTransition = hasTransition ((=) Epsilon)
    let private hasNonEpsilonTransition = hasTransition ((<>) Epsilon)

    let private convertToDfa (table: Transition<State> list) =
        /// Computes the list of transitions that can be taken from a state, skipping over epsilon transitions.
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

        let followTransitions origin transitions =
            // For each transition, get the states that can be reached from its destination by
            // non-epsilon transitions, and create transitions to them from the given state.
            transitions
            |> List.collect (getDest >> computeFollowSet table)
            |> List.distinct
            |> List.map (fun (input, dest) -> (origin, input), dest)

        let removeNondeterminism current table = 
            // Separate epsilon and non-epsilon transitions
            let epsilonTransitions, nonEpsilonTransitions =
                table
                |> transitionsFrom current
                |> List.partition (getInput >> (=) Epsilon)
            // Follow epsilon transitions to the next state with non-epsilon transitions
            let followedEpsilonTransitions = followTransitions current epsilonTransitions
            // Combine with followed epsilon transitions, and follow the destination state if it has epsilon transitions.
            nonEpsilonTransitions @ followedEpsilonTransitions
            |> followDestination

        /// Groups all transitions by input symbol, and merges states that can be reached
        /// by the same input symbol.
        let groupTransitions current transitions =
            let single, multiple =
                transitions
                |> List.distinct
                |> List.groupBy getInput
                |> List.partition (getDest >> List.length >> (=) 1)
            let single = List.collect snd single
            let merged =
                multiple
                |> List.where (fun (_, dests) -> List.length dests >= 2)
                |> List.map (fun (on, dests) ->
                    let mergedState =
                        dests
                        |> List.map getDest
                        |> List.distinct
                        |> State.merge
                    (current, on), mergedState)
            single @ merged

        let rec inner stack dfaTransitions =
            match stack with
            | [] -> List.ofSeq dfaTransitions
            | current::stack ->
                let transitionsFromCurrent =
                    table
                    |> removeNondeterminism current
                    |> groupTransitions current
                // Follow transitions that don't go to the current state or a state already in the stack
                let nextStack =
                    transitionsFromCurrent
                    |> List.map getDest
                    |> List.where ((<>) current)
                    |> List.append stack
                    |> List.distinct
                let nextTransitions =
                    transitionsFromCurrent
                    |> Set.ofList
                    |> Set.union dfaTransitions
                inner nextStack nextTransitions

        printf "NFA:\n\n"
        table
        |> List.indexed
        |> List.map (fun (i, ((fromState, m), toState)) -> sprintf "%d.\t(%s, %s)\t-> %s" i (string fromState) (string m) (string toState))
        |> String.concat "\n"
        |> Console.WriteLine

        inner [START] Set.empty

    let private buildStateMachine (features: IDictionary<string, Node>) sets target result environment =
        /// Gives nothing from the result.
        let inline giveNone result = None, result
        let takeState (states: State seq) =
            Seq.tail states, Seq.head states
        let featureTransformations =
            features
            |> Seq.map (fun kvp -> kvp.Key, getTransformations kvp.Value)
            |> Map.ofSeq

        /// <param name="states">An infinite sequence of functions that create states.</param>
        /// <param name="input">The list of input symbols (phonemes).</param>
        /// <param name="current">The last node added to the state machine being built.</param>
        /// <param name="transitions">Accumulates key-value pairs that will create the transition table.</param>
        /// <param name="transformations">Accumulates transformations produced by transitions.</param>
        /// <param name="fGive">Gives a node representing a single input symbol if inside the result section; otherwise, always gives None and the original result.</param>
        /// <param name="isAtBeginning">True if none of <c>input</c> has been processed yet; otherwise, false.</param>
        /// <param name="isSubtreeFinal">True if the last state in the subtree should be final.</param>
        let rec inner states (input: Node list) (result: Node list) current transitions transformations fGive isAtBeginning isSubtreeFinal =
            /// <summary>Gives a node representing a single input symbol from the result.</summary>
            /// <remarks>Utterance nodes are handled by returning an UtteranceNode containing the first character
            /// in the utterance, and replacing the head result node with an UtteranceNode containing the remainder.
            /// The only other nodes allowed in the result section, SetIdentifierNode and CompoundSetIdentifierNode,
            /// are taken from the result unmodified.</remarks>
            let inline giveFrom result =
                match result with
                | [] -> None, result
                // TODO: this is a code smell. maybe TaggedNode should have been its own type after all...
                | TaggedNode (_, node)::xs
                | node::xs ->
                    Some node, xs
            let inline giveTrue () = true
            let inline giveFalse () = false

            let endCata fEnd fNotEnd fNodeComplete =
                if not (isSubtreeFinal && fNodeComplete()) then
                    fNotEnd()
                else
                    fEnd()

            let isInputAtEnd input =
                endCata
                    giveTrue giveFalse
                    (fun () ->
                        match input with
                        | _::[] -> true
                        | _ -> false)

            let getNextState states isNodeComplete =
                let states, nextState = takeState states
                let nextState =
                    endCata
                        (fun _ -> State.makeFinal nextState)
                        (fun _ -> nextState)
                        (fun () -> isNodeComplete)
                states, nextState

            /// Creates a transition to a new state that matches an input symbol.
            let matchCharacter c =
                let states, target = getNextState states true
                let transitions = (on c current, target) :: transitions
                states, result, transitions, transformations, target

            let addTransformation transformations t maybeResult utterance =
                match maybeResult with
                | Some (UtteranceNode s) ->
                    (t, s) :: transformations
                | Some (CompoundSetIdentifierNode [TaggedNode (_, FeatureIdentifierNode (_, name))]) ->
                    match Map.tryFind utterance featureTransformations.[name] with
                    | Some result -> (t, result) :: transformations
                    | None -> transformations
                | _ -> transformations

            /// Creates a series of states and transitions that match each character of an utterance.
            let transformUtterance utterance =
                let rec innerTransformUtterance input result states transitions transformations next =
                    match input with
                    | [] ->
                        let resultNode, result = fGive result
                        let t = List.head transitions
                        let transformations = addTransformation transformations t resultNode utterance
                        states, result, transitions, transformations, next
                    | c::xs ->
                        let states, target = getNextState states (List.isEmpty xs)
                        let transitions = (on c next, target) :: transitions
                        innerTransformUtterance xs result states transitions transformations target

                innerTransformUtterance (List.ofSeq utterance) result states transitions transformations current

            /// Computes the intersection of a list of feature and set identifiers, and creates a
            /// tree of states and transitions that match each member of the resulting set.
            let transformSet setId =
                let states, lastState = getNextState states true

                let rec innerTransformSet states transitions curState tree =
                    match tree with
                    | PrefixTree.Root children
                    | PrefixTree.Node (_, _, children) ->
                        // Create states for each child and transitions to them
                        let states, transitions, transformations =
                            children
                            |> List.fold
                                (fun (states, transitions, transformations) n ->
                                    match n with
                                    | PrefixTree.Node (_, c, _) ->
                                        let states, nextState = takeState states
                                        let transitions = ((curState, Match.Char c), nextState) :: transitions
                                        let states, _, transitions, transformations, _ = innerTransformSet states transitions nextState n
                                        states, transitions, transformations
                                    | PrefixTree.Leaf _ ->
                                        let transitions = ((curState, Epsilon), lastState) :: transitions
                                        states, transitions, transformations
                                    | Root _ ->
                                        failwith "A Root should never have another Root as a descendant")
                                (states, transitions, transformations)
                        states, result, transitions, transformations, lastState
                    | PrefixTree.Leaf _ ->
                        states, result, transitions, transformations, curState

                PrefixTree.fromSetIntersection features sets setId
                |> innerTransformSet states transitions current

            let transformOptional children =
                let states, lastState = getNextState states (isInputAtEnd input)
                let states, result, transitions, transformations, subtreeLast =
                    inner states children result current transitions transformations fGive true false
                let transitions =
                    [ (current, Epsilon), lastState
                      (subtreeLast, Epsilon), lastState ]
                    @ transitions
                states, result, transitions, transformations, lastState

            let transformDisjunct branches =
                // Create a common exit point for all subtrees
                let states, lastState = getNextState states (isInputAtEnd input)
                // Build subtree for each branch
                let follows =
                    List.foldBack
                        (fun branch ((states, result, transitions, transformations, _)::_ as acc) ->
                            let isSubtreeFinal = isSubtreeFinal && isInputAtEnd input
                            let subtree = inner states branch result current transitions transformations fGive true isSubtreeFinal
                            subtree :: acc)
                        branches
                        [ states, result, transitions, transformations, current ]
                // The state of our NFA generator - states, result, transitions, transformations - is propagated to each
                // successive call to inner, and comes out here.
                let states, result, transitions, transformations, _ = List.head follows
                // Transition from last state of each subtree to lastState.
                // Reverse the list and take the tail first so we don't epsilon from current to lastState.
                let subtreeFinalToLastState =
                    follows
                    |> List.rev
                    |> List.tail
                    |> List.map (fun (_, _, _, _, subtreeFinal) -> (subtreeFinal, Epsilon), lastState)
                let transitions =
                    [ (current, Any), ERROR ] @ // Go to ERROR if we can't match anything
                    subtreeFinalToLastState @
                    transitions
                states, result, transitions, transformations, lastState

            let generatorState =
                match input with
                | [] ->
                    Done
                | TaggedNode (_, node)::_
                | node::_ ->
                    match node with
                    | BoundaryNode ->
                        let boundaryChar = if isAtBeginning then Special.START else Special.END
                        HasNext (matchCharacter boundaryChar)
                    | UtteranceNode utterance ->
                        HasNext (transformUtterance utterance)
                    | CompoundSetIdentifierNode setId ->
                        HasNext (transformSet setId)
                    | SetIdentifierNode _ as id ->
                        HasNext (transformSet [ id ])
                    | PlaceholderNode ->
                        HasNext (inner states target result current transitions transformations giveFrom true (isInputAtEnd input))
                    | OptionalNode children ->
                        HasNext (transformOptional children)
                    | DisjunctNode branches ->
                        HasNext (transformDisjunct branches)
                    | _ ->
                        failwithf "Unexpected '%s'" (string input.Head)

            match generatorState with
            | Done ->
                states, result, transitions, transformations, current
            | HasNext (states, result, transitions, transformations, theNext) ->
                inner states input.Tail result theNext transitions transformations fGive false isSubtreeFinal

        let initialStates = Seq.initInfinite (sprintf "q%d" >> State.make)
        let _, _, transitions, transformations, _ = inner initialStates environment result START List.empty List.empty giveNone true true
        let transitionTable =
            transitions
            |> List.rev
            //|> groupTransitions
            |> convertToDfa
            |> dict
        transitionTable

    let compile features sets rule =
        match untag rule with
        | RuleNode (target, result, environment) ->
            buildStateMachine features sets target result environment
        | _ ->
            raise (ArgumentException("Must be a RuleNode", "rule"))

    let test rule word =
        stateMachineConfig()
        |> withTransitions rule
        |> withStartState START
        |> withErrorState ERROR
        |> withInitialValue false
        |> onError (fun _ _ currentValue getNextValue -> currentValue |> getNextValue |> Restart)
        |> onTransition (fun _ input _ nextState value ->
            printfn "%s on %c" (string nextState) input
            nextState |> State.isFinal || value)
        |> onFinish (fun value -> value |> string |> Result.Ok)
        |> runStateMachine (sprintf "%c%s%c" Special.START word Special.END)
