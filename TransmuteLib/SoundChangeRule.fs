namespace TransmuteLib

open System
open System.Collections.Generic
open Node
open StateMachine

module SoundChangeRule =
    open System.Text

    type private Special =
        static member START = '␂'
        static member END = '␃'
        static member JOINER = '\ufeff' //'\u200d'

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
    type TransformationTable = Map<Transition<State>, string>

    type InputPosition =
        | InputInitial
        | InputNoninitial

    type SubtreePosition =
        | SubtreeNonfinal
        | SubtreeFinal

    type private RuleGeneratorState =
        | HasNext of State seq * Node list * Transition<State> list * Transformation list * State
        | Done

    type private TransitionType<'TState> =
        | MaybeDeterministic of Transition<'TState>
        | Deterministic of Transition<'TState>

    let private START = State.make "S"
    let private ERROR = State.make "Error"

    /// Returns true if both states are equal, or if one state is a merged state that contains the other.
    let inline private (<%>) x y =
        match x, y with
        | _ when x = y -> true
        | (State _ as s), MergedState children
        | MergedState children, (State _ as s) when List.contains s children -> true
        | _ -> false

    /// Matches the state or a merged state containing it
    let private (|IsOrContains|_|) x y =
        if x <%> y
            then Some x
            else None

    let private transitionsFrom state transitions =
        transitions
        |> List.choose (fun (From origin, input, To dest) ->
            match state with
            | IsOrContains origin _ ->
                Some (From state, input, To dest)
            | _ -> None)

    let private convertToDfa
        (table: Transition<State> list)
        (transformations: ((Origin<State> * InputSymbol * Destination<State>) * string) list) =
        let hasTransitionOn fCompare state =
            table
            |> List.exists (fun (From origin, input, _) -> state = origin && fCompare input)
        
        let hasEpsilonTransitionFrom state =
            table
            |> List.exists (fun (From origin, input, _ as t) -> input = OnEpsilon && origin <%> state)

        let replaceOrigin newOrigin (From _, input, To dest) = (newOrigin, input, dest)

        let hasEpsilonTransition = hasTransitionOn ((=) OnEpsilon)
        let hasNonEpsilonTransition = hasTransitionOn ((<>) OnEpsilon)

        let transformationsFrom =
            transformations
            |> Seq.map (fun (((From origin, input, _), _) as t) -> (origin, input), t)
            |> Map.ofSeq
        let transformationsTo =
            transformations
            |> Seq.map (fun (((_, input, To dest), _) as t) -> (input, dest), t)
            |> Map.ofSeq

        /// <summary>
        /// Computes the list of transitions that can be taken from a state, skipping over epsilon transitions.
        /// </summary>
        /// <returns>A set of input symbol and state tuples.</returns>
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
                            | From from, OnEpsilon, _ when from = x -> true
                            | _ -> false)
                        |> List.map getDest
                        |> set
                    // Non-ε transitions we can take from those states
                    let followTransitions =
                        transitions
                        |> List.filter (fun (From from, input, _) -> input <> OnEpsilon && (from <%> x || followStates.Contains(from)))
                        |> List.map (fun (_, input, To ``to``) -> input, ``to``)
                    let nextStates =
                        followStates
                        |> Seq.filter (fun s ->
                            transitions
                            |> List.exists (function
                                | From from, OnEpsilon, _ when from = s -> true
                                | _ -> false))
                        |> List.ofSeq
                    inner (nextStates @ xs) (followTransitions @ result)
            inner [state] []

        /// <summary>
        /// Recursively follows the destination of each transition to a state that has non-epsilon
        /// transitions, eliminating any that have only epsilon transitions.
        /// </summary>
        /// <returns>A list of deterministic transitions.</returns>
        let followDestination transitions =
            //let rec inner2 transitions acc =
            //    match transitions with
            //    | [] -> List.ofSeq acc
            //    | ((From origin, _, _) as t)::xs ->
            //        let followTransitions =
            //            table
            //            |> List.filter ((<>) t)
            //            |> List.filter (getOrigin >> (=) origin)
            //        let epsilonTransitions, nonepsilonTransitions =
            //            followTransitions
            //            |> List.partition (getInput >> (=) OnEpsilon)
            //        inner2 (epsilonTransitions @ xs) (nonepsilonTransitions @ acc)
            //    inner2 transitions []

            let rec inner acc search =
                match search with
                | [] -> List.ofSeq acc
                | _ ->
                    // Search for states that might have deterministic transitions from their destinations.
                    let successors =
                        search
                        |> List.collect (fun (From o, input, To d as t) ->
                            // For each transition T from O to D that is succeeded by a nondeterministic transition U,
                            // move the destination of T forwards to skip it. If any of these lead to deterministic transitions,
                            // they will be accumulated in the next iteration, along with final states.
                            let followedTransitions =
                                table
                                |> List.choose (function
                                    | From successor, OnEpsilon, To d2 as u
                                        when successor <%> d && hasEpsilonTransition d2 ->
                                        Some (MaybeDeterministic (From o, input, To d2))
                                    | _ -> None)
                            // Keep the original transition T if D is final or has deterministic transitions
                            let originalTransition =
                                if State.isFinal d || input <> OnEpsilon || hasNonEpsilonTransition d
                                    then [Deterministic t]
                                    else []
                            originalTransition @ followedTransitions)
                        //|> List.distinct
                    // Follow transitions to states we haven't already been to that have non-deterministic transitions
                    let nextTransitions =
                        //|> Seq.where (fun ((_, _, To d) as t) ->
                        //    not <| List.contains t search && hasEpsilonTransition d)
                        successors
                        |> Seq.choose (function
                            | MaybeDeterministic u when not (List.contains u search) -> Some u
                            | _ -> None)
                        |> List.ofSeq
                    // Accumulate transitions to states that are final or have deterministic transitions
                    let nextAcc =
                        successors
                        //|> Seq.where (fun (_, _, To d) -> State.isFinal d || hasNonEpsilonTransition d)
                        |> Seq.choose (function
                            | Deterministic t -> Some t
                            | _ -> None)
                        |> Set.ofSeq
                        |> Set.union acc
                    inner nextAcc nextTransitions

            inner (Set.empty) transitions
            //inner2

        /// <summary>
        /// For each transition, get the states that can be reached from its destination by
        /// non-epsilon transitions, and create transitions to them from the given state.
        /// </summary>
        /// <returns>A list of deterministric transitions.</returns>
        let followEpsilonTransitions origin transitions =
            transitions
            |> List.collect (getDest >> computeFollowSet table)
            |> List.distinct
            |> List.map (fun (input, dest) -> From origin, input, To dest)

        /// Replaces epsilon transitions originating from current with all possible deterministic transitions.
        ///
        /// Also replaces destination states that have epsilon transition with states that can be reached deterministically.
        let removeNondeterminism current transitions = 
            // Separate epsilon and non-epsilon transitions
            let epsilonTransitions, nonEpsilonTransitions =
                transitions
                |> List.partition (getInput >> (=) OnEpsilon)
            // Follow epsilon transitions to the next state with non-epsilon transitions
            let followedEpsilonTransitions = followEpsilonTransitions current epsilonTransitions
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
                |> List.partition (snd >> List.length >> (=) 1)
            let single = List.collect snd single
            let merged =
                multiple
                |> List.map (fun (on, dests) ->
                    let mergedState =
                        dests
                        |> List.map getDest
                        |> List.distinct
                        |> State.merge
                    From current, on, To mergedState)
            single @ merged

        let rec inner stack dfaTransitions =
            match stack with
            | [] -> List.ofSeq dfaTransitions
            | current::stack ->
                // transitions from current state -> skip lambdas -> group by symbol
                let transitionsFromCurrent =
                    table
                    |> transitionsFrom current
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
        |> List.map (fun (i, (From origin, input, To dest)) ->
            let t = sprintf "(%O, %O)" origin input
            sprintf "%d.\t%-35s-> %O" i t dest)
        |> String.concat "\n"
        |> Console.WriteLine

        (inner [START] Set.empty), []

    let private buildStateMachine (features: Map<string, Node>) sets target result environment =
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
        /// <param name="fGiveInput">Gives a node representing a single input symbol if inside the result section; otherwise, always gives None and the original result.</param>
        /// <param name="isAtBeginning">True if none of <c>input</c> has been processed yet; otherwise, false.</param>
        /// <param name="isSubtreeFinal">True if the last state in the subtree should be final.</param>
        let rec inner states (input: Node list) (result: Node list) current transitions transformations fGiveInput inputPosition subtreePosition =
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
                if not (subtreePosition = SubtreeFinal && fNodeComplete()) then
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
                let states, target = getNextState states (isInputAtEnd input) //true
                let transitions = (From current, OnChar c, To target) :: transitions
                states, result, transitions, transformations, target

            /// If possible, adds a transformation for an utterance.
            let addTransformation transformations t maybeResult utterance =
                match maybeResult with
                // replace with another utterance
                | Some (UtteranceNode s) ->
                    (t, s) :: transformations
                // transform by flipping one feature
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
                        let resultNode, result = fGiveInput result
                        let transformations = addTransformation transformations (List.head transitions) resultNode utterance
                        states, result, transitions, transformations, next
                    | c::xs ->
                        let states, target = getNextState states (List.isEmpty xs)
                        let transitions = (From next, OnChar c, To target) :: transitions
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
                                        let transitions = (From curState, OnChar c, To nextState) :: transitions
                                        let states, _, transitions, transformations, _ = innerTransformSet states transitions nextState n
                                        states, transitions, transformations
                                    | PrefixTree.Leaf _ ->
                                        let transitions = (From curState, OnEpsilon, To lastState) :: transitions
                                        states, transitions, transformations
                                    | PrefixTree.Root _ ->
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
                    inner states children result current transitions transformations fGiveInput InputNoninitial SubtreeNonfinal
                let transitions =
                    [ From current, OnEpsilon, To lastState
                      From subtreeLast, OnEpsilon, To lastState ]
                    @ transitions
                states, result, transitions, transformations, lastState

            let transformDisjunct branches =
                // Create a common exit point for all subtrees
                let states, lastState = getNextState states (isInputAtEnd input)
                // Build subtree for each branch
                let follows =
                    List.foldBack
                        (fun branch ((states, result, transitions, transformations, _)::_ as acc) ->
                            //let isSubtreeFinal = subtreePosition = SubtreeFinal && isInputAtEnd input
                            let innerSubtreePosition =
                                if subtreePosition = SubtreeFinal && isInputAtEnd input
                                    then SubtreeFinal
                                    else SubtreeNonfinal
                            let subtree = inner states branch result current transitions transformations fGiveInput InputNoninitial innerSubtreePosition //subtreePosition
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
                    |> List.map (fun (_, _, _, _, subtreeFinal) -> From subtreeFinal, OnEpsilon, To lastState)
                let transitions =
                    [ From current, OnAny, To ERROR ] @ // Go to ERROR if we can't match anything
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
                        let boundaryChar =
                            match inputPosition with
                            | InputInitial -> Special.START
                            | InputNoninitial -> Special.END
                        HasNext (matchCharacter boundaryChar)
                    | UtteranceNode utterance ->
                        HasNext (transformUtterance utterance)
                    | CompoundSetIdentifierNode setId ->
                        HasNext (transformSet setId)
                    | SetIdentifierNode _ as id ->
                        HasNext (transformSet [ id ])
                    | PlaceholderNode ->
                        let subtreePosition =
                            match isInputAtEnd input with
                            | true -> SubtreeFinal
                            | false -> SubtreeNonfinal
                        HasNext (inner states target result current transitions transformations giveFrom InputNoninitial subtreePosition)
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
                inner states input.Tail result theNext transitions transformations fGiveInput InputInitial subtreePosition

        let initialStates = Seq.initInfinite (sprintf "q%d" >> State.make)
        let _, _, transitions, transformations, _ = inner initialStates environment result START List.empty List.empty giveNone InputNoninitial SubtreeFinal
        let transitions, transformations =
            (List.rev transitions, transformations)
            //|> groupTransitions
            ||> convertToDfa

        let transitionTable =
            transitions
            |> Seq.map (fun (From origin, input, To dest) -> (origin, input), dest)
            |> Map.ofSeq
        (Map.ofSeq transformations), transitionTable

    let compile features sets rule =
        match untag rule with
        | RuleNode (target, result, environment) ->
            buildStateMachine features sets target result environment
        | _ ->
            invalidArg "rule" "Must be a RuleNode"

    let test rule word =
        stateMachineConfig()
        |> withTransitions rule
        |> withStartState START
        |> withErrorState ERROR
        |> withInitialValue false
        |> onError (fun _ _ currentValue getNextValue -> currentValue |> getNextValue |> Restart)
        |> onTransition (fun _ _ input _ nextState value ->
            printfn "%O on %c" nextState input
            value || State.isFinal nextState)
        |> onFinish (fun value -> value |> string |> Result.Ok)
        |> runStateMachine (sprintf "%c%s%c" Special.START word Special.END)

    type RuleMachineState = {
        transformationTable: Map<Transition<State>, string>
        /// The output so far
        output: string list
        /// The output so far of the current transformation
        buffer: string list
        /// The untransformed output that would be produced if the transformation fails
        undoBuffer: string list
    }

    let transform transformations transitions word =
        stateMachineConfig()
        |> withTransitions transitions
        |> withStartState START
        |> withErrorState ERROR
        |> withInitialValue { transformationTable = transformations; output = []; buffer = []; undoBuffer = [] }
        |> onError (fun input _ currentValue getNextValue ->
            Restart {
                currentValue with
                    output = string input :: currentValue.undoBuffer @ currentValue.output
                    buffer = []
                    undoBuffer = []
            })
        |> onTransition (fun transition _ input _ nextState value ->
            printfn "%O on %c" nextState input
            { value with
                undoBuffer = (string input) :: value.undoBuffer
                buffer =
                    match Map.tryFind transition value.transformationTable with
                    | Some transformation -> transformation :: value.buffer
                    | None -> value.buffer
            })
        |> onFinish (fun value ->
            value |> string |> Result.Ok)
        |> runStateMachine (sprintf "%c%s%c" Special.START word Special.END)
