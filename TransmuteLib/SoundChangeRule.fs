// TODO: cleanup (make nested functions module-level and private)

namespace TransmuteLib

open Node
open StateMachine
open System

module SoundChangeRule =
    type private Special =
        static member START = '␂'
        static member END = '␃'
        static member JOINER = '\ufeff' //'\u200d'

    type StateType = Final | NonFinal
    type Segment = EnvironmentSegment | TargetSegment

    type State =
        | State of name: string * segment: Segment * stateType: StateType
        | MergedState of State list
        with
            /// Returns the state's name.
            static member name = function
                | State (name, _, _) -> name
                | MergedState states ->
                    states
                    |> Seq.map State.name
                    |> String.concat (string Special.JOINER)

            /// Returns the ordinal part of a state's name (e.g. "q5" -> 5), -1 if the state name contains no ordinal part,
            /// or throws an exception if given a merged state.
            static member ord = function
                | State (name, _, _) ->
                    match name.[1..] with
                    | "" -> -1
                    | x -> int x
                | MergedState _ ->
                    failwith "Merged states have no ordinal"

            /// Creates a non-final state marked as matching input in the target segment.
            static member make name = State (name, TargetSegment, NonFinal)
        
            /// Marks a state as being final.
            static member makeFinal = function
                | State (name, isTarget, _) ->
                    State (name, isTarget, Final)
                | MergedState _ as state ->
                    failwithf "%s is a merged state; it cannot be made final" (string state)


            /// Marks a state as corresponding to input matched in the environment segment.
            static member makeEnvironment = function
                | State (name, _, isFinal) ->
                    State (name, EnvironmentSegment, isFinal)
                | MergedState states ->
                    let states =
                        states
                        |> List.map (fun (State (name, _, isFinal)) -> State (name, EnvironmentSegment, isFinal))
                    MergedState states

            /// Merges a list of states into one merged state.
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

            /// Returns a boolean indicating whether the state is final.
            static member isFinal = function
                | State (_, _, Final) -> true
                | State (_, _, NonFinal) -> false
                | MergedState states ->
                    List.exists State.isFinal states

            /// Returns a boolean indicating whether the state corresponds to input matched in the environment segment.
            static member isEnvironment = function
                | State (_, EnvironmentSegment, _) -> true
                | State (_, TargetSegment, _) -> false
                | MergedState states ->
                    List.exists State.isEnvironment states

            override this.ToString() =
                if State.isFinal this
                    then State.name this |> sprintf "(%s)"
                    else State.name this |> sprintf "%s"

    type Transformation = Transition<State> * string

    let inline private getOrigin ((From tOrigin, _, _), _) = tOrigin

    type TransitionResult =
        | NoOutput
        | Produces of string

    // Transition augmented with an optional transformation
    type Transition' = Transition<State> * TransitionResult

    // Combines a transition list and a transformation list into a transition + optional transformation list
    let private augment (transitions: Transition<State> list) (transformations: Transformation list) =
        let transformationsByTransition =
            transformations
            |> List.groupBy (fun (transition, _) -> transition)
            |> List.map (fun (key, tfs) -> (key, List.map snd tfs))
            |> Map.ofList
        transitions
        |> List.map (fun t ->
            let transformation =
                match Map.tryFind t transformationsByTransition with
                | None -> NoOutput
                | Some [tf] -> Produces tf
                | Some tfs -> failwithf "Transition %O has %d transformations; it should have 0 or 1" t tfs.Length
            t, transformation)

    //type TransformationTable = Map<Transition<State>, string>

    type private InputPosition =
        | InputInitial
        | InputNoninitial

    type private InputNode =
        | Environment
        | Placeholder

    type private SubtreePosition =
        | SubtreeNonfinal
        | SubtreeFinal

    type private RuleGeneratorState =
        | HasNext of states: State seq * result: Node list * transitions: Transition<State> list * transformations: Transformation list * current: State
        | Done

    type private TransitionType<'TState> =
        /// A transition with a destination state that needs to be checked for deterministic transitions.
        | MaybeDeterministic of (Transition<'TState> * TransitionResult)
        /// A transition with a destination state that either has deterministic transitions or just doesn't have any nondeterministic ones.
        | Deterministic of (Transition<'TState> * TransitionResult)

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

    let private (|IsTarget|_|) state =
        match state with
        | State (_, TargetSegment, _)
        | MergedState ((State (_, TargetSegment, _))::_) ->
            Some state
        | _ ->
            None

    let private (|IsEnvironment|_|) state =
        match state with
        | State (_, EnvironmentSegment, _)
        | MergedState ((State (_, EnvironmentSegment, _))::_) ->
            Some state
        | _ ->
            None

    let private transitionsFrom state transitions =
        transitions
        |> List.choose (fun (((From origin, _, _), _) as t) ->
            match state with
            | IsOrContains origin _ ->
                Some t
            | _ -> None)

    // TODO: pull this out into its own module in another file
    let private convertToDfa (table: Transition<State> list) (transformations: Transformation list) =
        let table = augment table transformations

        let hasTransitionOn fCompare state =
            List.exists
                (fun ((From origin, input, _), _) -> state = origin && fCompare input)
                table

        let hasEpsilonTransition = hasTransitionOn ((=) OnEpsilon)
        let hasNonEpsilonTransition = hasTransitionOn ((<>) OnEpsilon)

        let allTransitionsDeterministic origin =
            table
            |> Seq.filter (function
                | (From o, OnEpsilon, _), _ when o = origin -> true
                | _ -> false)
            |> Seq.isEmpty

        let inline getDest (transition, _) = StateMachine.getDest transition
        let inline getInput (transition, _) = StateMachine.getInput transition

        /// <summary>
        /// Computes the list of transitions that can be taken from a state, skipping over epsilon transitions.
        /// </summary>
        /// <returns>A set of input symbol and state tuples.</returns>
        let computeFollowSet transitions state =
            let rec inner transitions states result =
                match states with
                | [] ->
                    List.rev result
                | x::xs ->
                    // States to which we can ε transition from x
                    let followStates =
                        transitions
                        |> List.filter (function
                            | (From origin, OnEpsilon, _), _ when origin = x -> true
                            | _ -> false)
                        |> List.map getDest
                        |> set
                    // Non-ε transitions we can take from those states
                    let followTransitions =
                        transitions
                        |> List.filter
                            (fun ((From origin, input, _), _) ->
                                input <> OnEpsilon
                                && (origin <%> x
                                    || Set.contains origin followStates))
                        |> List.map (fun ((_, input, To dest), result) -> input, dest, result)
                    let nextStates =
                        followStates
                        |> Seq.filter (fun s ->
                            transitions
                            |> List.exists (function
                                | (From origin, OnEpsilon, _), _ when origin = s -> true
                                | _ -> false))
                        |> List.ofSeq
                    inner transitions (nextStates @ xs) (followTransitions @ result)
            inner transitions [state] []

        // TODO: refactor this 8 level indented beast
        /// <summary>
        /// Recursively follows the destination of each transition to a state that has non-epsilon
        /// transitions, eliminating any that have only epsilon transitions.
        /// </summary>
        /// <returns>A list of deterministic transitions.</returns>
        let followDestination current transitions =
            let rec inner acc search =
                match search with
                | [] -> List.ofSeq acc
                | _ ->
                    // Search for states that might have deterministic transitions from their destinations.
                    let successors =
                        search
                        |> List.collect (fun ((From o, input, To d), result as t) ->
                            // For each transition T from O to D that is succeeded by a nondeterministic transition U,
                            // move the destination of T forwards to skip it. If any of these lead to deterministic transitions,
                            // they will be accumulated in the next iteration, along with final states.
                            let followedTransitions =
                                table
                                |> List.choose (function
                                    | (From successor, OnEpsilon, To d2), uResult as u
                                        //when successor <%> d && hasEpsilonTransition d2 ->
                                        when successor <%> d ->
                                        let result =
                                            match result, uResult with
                                            | (Produces _ as r), NoOutput
                                            | NoOutput, (Produces _ as r) ->
                                                r
                                            | NoOutput, NoOutput ->
                                                NoOutput
                                            | Produces _, Produces _ ->
                                                failwith "Both transitions have transformation!"
                                        Some (MaybeDeterministic ((From current, input, To d2), result))
                                    | _ -> None)
                            // Keep the original transition T if D is final or has deterministic transitions
                            let originalTransition =
                                if (allTransitionsDeterministic d || hasNonEpsilonTransition d)
                                    && (State.isFinal d || input <> OnEpsilon)
                                    then [Deterministic t]
                                    else []
                            originalTransition @ followedTransitions)
                    // Follow transitions to states we haven't already been to that have non-deterministic transitions
                    let nextTransitions =
                        successors
                        |> Seq.choose (function
                            | MaybeDeterministic u when not (List.contains u search) -> Some u
                            | _ -> None)
                        |> List.ofSeq
                    // Accumulate transitions to states that are final or have deterministic transitions
                    let nextAcc =
                        successors
                        |> Seq.choose (function
                            | Deterministic t -> Some t
                            | _ -> None)
                        |> Set.ofSeq
                        |> Set.union acc
                    inner nextAcc nextTransitions

            inner (Set.empty) transitions

        /// <summary>
        /// For each transition, get the states that can be reached from its destination by
        /// non-epsilon transitions, and create transitions to them from the given state.
        /// </summary>
        /// <returns>A list of deterministric transitions.</returns>
        let followEpsilonTransitions origin transitions =
            transitions
            |> List.collect (getDest >> computeFollowSet table)
            |> List.distinct
            |> List.map (fun (input, dest, result) -> (From origin, input, To dest), result)

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
            let transitions =
                nonEpsilonTransitions @ followedEpsilonTransitions
                |> followDestination current
            transitions

        /// Groups all transitions by input symbol, and merges states that can be reached
        /// by the same input symbol.
        let groupTransitions current (transitions: (Transition<State> * TransitionResult) list) =
            let single, multiple =
                transitions
                |> List.distinct
                |> List.groupBy getInput
                |> List.partition (snd >> List.length >> (=) 1)
            let single = List.collect snd single
            let merged =
                multiple
                |> List.map (fun (on, dests) ->
                    let mergedDest =
                        dests
                        |> List.map getDest
                        |> State.merge
                    let production =
                        (NoOutput, dests)
                        ||> List.fold (fun out (_, result) ->
                            match out, result with
                            | Produces a, Produces b when a <> b ->
                                failwithf "Merged state %O has multiple productions! (%O, %O)" mergedDest out result
                            | NoOutput, (Produces _ as result) ->
                                result
                            | _ ->
                                out)
                    (From current, on, To mergedDest), production)
            single @ merged

        let rec convertToDfa' stack dfaTransitions =
            match stack with
            | [] -> 
                dfaTransitions
                |> List.ofSeq
            | x::stack when x = ERROR ->
                convertToDfa' stack dfaTransitions
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
                convertToDfa' nextStack nextTransitions

        //printf "NFA:\n\n"
        //table
        //|> List.sortBy (fun ((From origin, input, dest), result) -> (State.ord origin, input, dest), result)
        //|> List.indexed
        //|> List.map (fun (i, ((From origin, input, To dest), result)) ->
        //    let t = sprintf "(%O, %O)" origin input
        //    sprintf "%d.\t%-25s-> %O, %O" i t dest result)
        //|> String.concat "\n"
        //|> Console.WriteLine

        convertToDfa' [START] Set.empty

    let private buildStateMachine (features: Map<string, Node>) sets target result environment =
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
        let rec buildStateMachine' states (input: Node list) (result: Node list) transitions transformations inputNode inputPosition subtreePosition parentPosition current =
            /// <summary>
            /// When visiting result nodes, consumes a single input symbol and returns a node representing it.
            /// When visiting environment nodes or when there is no result left to give, consumes nothing and gives nothing.
            /// </summary>
            /// <remarks>
            /// Utterance nodes are consumed one symbol at a time by returning an UtteranceNode containing the first
            /// character in the utterance, and replacing the head result node with an UtteranceNode containing the
            /// remainder. The only other nodes allowed in the result section, SetIdentifierNode and
            /// CompoundSetIdentifierNode, are taken from the result unmodified.
            /// </remarks>
            let giveResult result =
                match inputNode, result with
                | Environment, _
                | Placeholder, [] ->
                    None, result
                | Placeholder, TaggedNode (_, node)::xs ->
                //| Placeholder, node::xs
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
                let nextState =
                    match inputNode with
                    | Environment -> State.makeEnvironment nextState
                    | _ -> nextState
                states, nextState

            /// Creates a transition to a new state that matches an input symbol.
            let matchCharacter c =
                let states, target = getNextState states (isInputAtEnd input) //true
                let transitions = (From current, OnChar c, To target) :: transitions
                states, result, transitions, transformations, target

            /// If possible, adds a transformation for an utterance.
            let addUtteranceTransformation transformations t maybeResult utterance =
                match maybeResult with
                // replace with another utterance
                | Some (UtteranceNode s) ->
                    (t, s) :: transformations
                // transform by flipping one feature
                | Some (CompoundSetIdentifierNode [TaggedNode (_, FeatureIdentifierNode (isPresent, name))]) ->
                    let additions, removals = featureTransformations.[name]
                    let searchMap = if isPresent then additions else removals
                    match Map.tryFind utterance searchMap with
                    | Some result -> (t, result) :: transformations
                    | None -> transformations
                | _ -> transformations

            /// Creates a series of states and transitions that match each character of an utterance.
            let transformUtterance utterance isSegmentComplete =
                // TODO: make sure that input gets added to buffer when outside placeholder node
                let rec innerTransformUtterance input result states transitions transformations next =
                    match input with
                    | [] ->
                        let resultNode, result = giveResult result
                        let transformations = addUtteranceTransformation transformations (List.head transitions) resultNode utterance
                        states, result, transitions, transformations, next
                    | c::xs ->
                        let states, target = getNextState states (isSegmentComplete && List.isEmpty xs)
                        let transitions = (From next, OnChar c, To target) :: transitions
                        innerTransformUtterance xs result states transitions transformations target

                innerTransformUtterance (List.ofSeq utterance) result states transitions transformations current

            let addSetTransformation value transition transformations result =
                let resultNode, result = giveResult result
                let transformations =
                    match resultNode with
                    | Some (UtteranceNode utterance) ->
                        (transition, utterance) :: transformations
                    | Some (CompoundSetIdentifierNode (Node.Untag (FeatureIdentifierNode (isPresent, name), _)::[])) ->
                        let additions, removals = featureTransformations.[name]
                        let searchMap = if isPresent then additions else removals
                        match Map.tryFind value searchMap with
                        | Some result -> (transition, result) :: transformations
                        | None -> transformations
                    | Some (CompoundSetIdentifierNode _) ->
                        failwith "Only one feature may be transformed at a time"
                    | _ ->
                        transformations
                transformations, result

            /// Computes the intersection of a list of feature and set identifiers, and creates a
            /// tree of states and transitions that match each member of the resulting set.
            let transformSet setDesc isEndOfSubtree =
                let states, terminator = getNextState states isEndOfSubtree

                let rec innerTransformSet states transitions transformations curState tree =
                    match tree with
                    | PrefixTree.Root children
                    | PrefixTree.Node (_, _, children) ->
                        // Create states for each child and transitions to them
                        let states, result, transitions, transformations =
                            List.fold
                                (fun (states, result, transitions, transformations) n ->
                                    match n with
                                    | PrefixTree.Node (_, c, _) ->
                                        // Create a node for this input symbol and a transition to it, and visit the children.
                                        let states, nextState = takeState states
                                        let transitions = (From curState, OnChar c, To nextState) :: transitions
                                        let states, _, transitions, transformations, _ = innerTransformSet states transitions transformations nextState n
                                        states, result, transitions, transformations
                                    | PrefixTree.Leaf value ->
                                        // No more input symbols. Create a transition to the terminator state.
                                        let t = From curState, OnEpsilon, To terminator
                                        let nextTransformations, nextResult = addSetTransformation value t transformations result
                                        let nextTransitions = t :: transitions
                                        states, nextResult, nextTransitions, nextTransformations
                                        //if nextTransformations = transformations then
                                        //    states, result, nextTransitions, transformations
                                        //else
                                        //    states, nextResult, nextTransitions, nextTransformations
                                    | PrefixTree.Root _ ->
                                        failwith "A Root should never be the descendant of another node")
                                (states, result, transitions, transformations)
                                children
                        states, result, transitions, transformations, terminator
                    | PrefixTree.Leaf _ ->
                        states, result, transitions, transformations, curState

                setDesc
                |> PrefixTree.fromSetIntersection features sets
                |> innerTransformSet states transitions transformations current

            let transformOptional children =
                let states, lastState = getNextState states (isInputAtEnd input)
                let states, result, transitions, transformations, subtreeLast =
                    buildStateMachine' states children result transitions transformations inputNode InputNoninitial SubtreeNonfinal subtreePosition current
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
                            let subtree =
                                buildStateMachine' states branch result transitions transformations inputNode inputPosition innerSubtreePosition subtreePosition current //subtreePosition
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
                | TaggedNode (_, node)::xs ->
                    let isEndOfSubtree =
                        SubtreeFinal = parentPosition
                        && SubtreeFinal = subtreePosition
                        && List.isEmpty xs
                    match node with
                    | BoundaryNode ->
                        let boundaryChar =
                            match inputPosition with
                            | InputInitial -> Special.START
                            | InputNoninitial -> Special.END
                        HasNext (matchCharacter boundaryChar)
                    | UtteranceNode utterance ->
                        HasNext (transformUtterance utterance isEndOfSubtree)
                    | CompoundSetIdentifierNode setDesc ->
                        HasNext (transformSet setDesc isEndOfSubtree)
                    | SetIdentifierNode _ as id ->
                        HasNext (transformSet [ id ] isEndOfSubtree)
                    | PlaceholderNode ->
                        let subsubtreePosition =
                            match isInputAtEnd input with
                            | true -> SubtreeFinal
                            | false -> SubtreeNonfinal
                        HasNext (buildStateMachine' states target result transitions transformations Placeholder InputNoninitial subsubtreePosition subtreePosition current)
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
                let input, subtreePosition =
                    // We're consuming the top symbol, so the next one we consume will produce final transitions.
                    match input with
                    | [_; x] when parentPosition = SubtreeFinal ->
                        [x], SubtreeFinal
                    | _::xs ->
                        xs, SubtreeNonfinal
                buildStateMachine' states input result transitions transformations inputNode InputNoninitial subtreePosition parentPosition theNext

        let initialStates = Seq.initInfinite (sprintf "q%d" >> State.make)

        let initialSubtreePosition =
            match environment with
            | [_] -> SubtreeFinal
            | _ -> SubtreeNonfinal

        let _, _, transitions, transformations, _ =
            buildStateMachine' initialStates environment result List.empty List.empty Environment InputInitial initialSubtreePosition SubtreeFinal START

        let dfa = convertToDfa (List.rev transitions) transformations

        let dfaTransformations =
            dfa
            |> List.choose (function
                | t, Produces result -> Some (t, result)
                | _, NoOutput -> None)

        let dfaTransitionTable =
            dfa
            |> Seq.map (fun ((From origin, input, To dest), result) -> (origin, input), dest)
            |> Map.ofSeq

        dfaTransitionTable, (Map.ofSeq dfaTransformations)

    let compile features sets rule =
        match untag rule with
        | RuleNode (target, result, environment) ->
            buildStateMachine features sets target result environment
        | _ ->
            invalidArg "rule" "Must be a RuleNode"

    let test (transitions, transformations) word =
        stateMachineConfig()
        |> withTransitions transitions
        |> withStartState START
        |> withErrorState ERROR
        |> withInitialValue false
        |> onError (fun _ _ _ currentValue getNextValue -> currentValue |> getNextValue |> Restart)
        |> onTransition (fun _ _ _ input _ nextState value ->
            printfn "%O on %c" nextState input
            value || State.isFinal nextState)
        |> onFinish (fun value -> value |> string |> Result.Ok)
        |> runStateMachine (sprintf "%c%s%c" Special.START word Special.END)

    /// Returns the original value if input is the special begin or end symbol.
    /// Otherwise, returns the result of fTransform.
    let private specialSymbolCata input value fTransform =
        match input with
        | x when x = Special.START || x = Special.END ->
            value
        | _ ->
            fTransform()

    /// An input symbol tagged with the segment it came from and its position.
    type EnvironmentString =
        | FromTarget of input: string * position: int
        | FromEnvironment of input: string * position: int
        with
            /// Gets the input symbol.
            static member get = function
                | FromEnvironment (s, _)
                | FromTarget (s, _) -> s

            /// Adds the input symbol c to xs.
            static member add c xs =
                specialSymbolCata
                    c xs
                    (fun () -> string c :: xs)

            /// Tags the input symbol c with its position and adds it to xs.
            static member addWithPosition c position xs =
                specialSymbolCata
                    c xs
                    (fun () -> (string c, position) :: xs)

            /// Tags the input symbol as originating from the target segment and adds it to xs.
            static member addTarget position c xs =
                specialSymbolCata
                    c xs
                    (fun () -> (FromTarget (string c, position)) :: xs)

            /// Tags the input symbol as originating from the environment segment and adds it to xs.
            static member addEnvironment position c xs =
                specialSymbolCata
                    c xs
                    (fun () -> (FromEnvironment (string c, position)) :: xs)

            /// Concatenates all inputs in s to xs.
            static member concatAll inputs xs =
                List.map EnvironmentString.get inputs @ xs

            /// Concatenates all environment segment inputs in s to xs.
            static member concatEnvironment s xs =
                let environmentInputs =
                    s
                    |> List.choose (function
                        | FromEnvironment (s', _) ->
                            Some s'
                        | _ ->
                            None)
                environmentInputs @ xs

            /// Concatenates all position-tagged environment segment inputs to xs and sorts by position.
            static member concatEnvironmentWithPosition s xs =
                let environmentInputs =
                    s
                    |> List.choose (function
                        | FromEnvironment (s', p) ->
                            Some (s', p)
                        | _ ->
                            None)
                environmentInputs @ xs
                |> List.sortByDescending snd

    type RuleMachineState = {
        /// Indicates whether the rule has begun to match to the input.
        isPartialMatch: bool

        /// Indicates whether the last state visited was a final state.
        wasLastFinal: bool

        /// The last input position visited.
        lastOutputOn: int option

        /// The current production.
        production: (string * int) list

        /// The undo buffer.
        undo: EnvironmentString list

        /// The output buffer.
        output: string list
    }

    let transform verbose (transitions, transformations) word =
        stateMachineConfig()
        |> withTransitions transitions
        |> withStartState START
        |> withErrorState ERROR
        |> withInitialValue {
            isPartialMatch = false
            wasLastFinal = false
            lastOutputOn = None
            production = []
            undo = []
            output = []
        }
        |> onError (fun position input current value _ ->
            let nextOutput =
                match value.isPartialMatch, value.wasLastFinal with
                // The rule failed to match
                | true, false ->
                    EnvironmentString.concatAll value.undo value.output
                // The rule failed to match completely, but what did match was enough to commit the production.
                | true, true ->
                    // undo and production will never be populated at the same time
                    (List.map fst value.production) @ EnvironmentString.concatAll value.undo value.output
                // The rule has not yet begun to match.
                | false, _ ->
                    EnvironmentString.add input value.output
            if verbose then
                printf "x %2d %c %8O: " position input value.lastOutputOn
                printfn "%-20s | %O %A %A %A"
                    (sprintf "Error at %O" current)
                    position
                    []
                    []
                    nextOutput
            Restart {
                value with
                    isPartialMatch = false
                    lastOutputOn = Some position
                    production = []
                    undo = []
                    output = nextOutput
            })
        |> onTransition (fun position transition _ input current nextState value ->
            let { lastOutputOn = lastOutputOn; production = production; undo = undo; output = output } = value
            if verbose then
                printf "√ %2d %c %8O: " position input lastOutputOn
            let isNextFinal = State.isFinal nextState
            let tf = Map.tryFind transition transformations
            let nextUndo, nextProduction, nextOutput =
                match isNextFinal, nextState, tf with
                // Completed match in environment segment with no transformation.
                // Commit the input symbol and combine the undo and production buffers.
                | true, IsEnvironment _, None ->
                    let nextProduction =
                        production
                        |> EnvironmentString.addWithPosition input position
                        |> EnvironmentString.concatEnvironmentWithPosition undo
                    [], nextProduction, output
                // Completed match with a transformation.
                // Set production to the output of the transformation and add undo to output.
                | true, IsEnvironment _, Some tf'
                | true, IsTarget _, Some tf' ->
                    let nextProduction = [tf', position]
                    [], nextProduction, EnvironmentString.concatEnvironment undo output
                // Partial match in environment segment.
                // Add input to undo.
                | false, IsEnvironment _, _ ->
                    let nextUndo = EnvironmentString.addEnvironment position input undo
                    nextUndo, production, output
                // Partial match in target segment with a transformation.
                // Set production to the output of the transformation and add input to undo.
                | false, IsTarget _, Some tf' ->
                    let nextProduction = [tf', position]
                    let nextUndo = EnvironmentString.addTarget position input undo
                    nextUndo, nextProduction, output
                // Partial match in target segment with no transformation.
                // Add input to undo.
                | false, IsTarget _, None ->
                    let nextUndo = EnvironmentString.addTarget position input undo
                    nextUndo, production, output
                | _ ->
                    undo, production, output
            //let nextBuffer = Buffer (Some position, nextProduction, nextUndo, nextOutput)
            if verbose then
                printfn "%-20s | %O %A %A %A"
                    (sprintf "%O -> %O" current nextState)
                    position
                    nextProduction
                    nextUndo
                    nextOutput
            { value with
                isPartialMatch = true
                wasLastFinal = isNextFinal
                lastOutputOn = Some position
                production = nextProduction
                undo = nextUndo
                output = nextOutput
            })
        |> onFinish (fun value ->
            // Flush last production
            let nextOutput =
                match value.wasLastFinal, value.production with
                | false, _ -> value.output
                | true, [] -> (List.map EnvironmentString.get value.undo) @ value.output
                | true, _ -> (List.map fst value.production) @ value.output
            nextOutput
            |> List.rev
            |> String.concat String.Empty)
        |> runStateMachine (string Special.START + word + string Special.END)
