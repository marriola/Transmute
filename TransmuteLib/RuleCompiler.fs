// TODO: cleanup (make nested functions module-level and private)

namespace TransmuteLib

module RuleCompiler =
    type private InputPosition =
        | InputInitial
        | InputNoninitial

    type private InputNode =
        | Environment
        | Placeholder

    type private SubtreePosition =
        | SubtreeNonfinal
        | SubtreeFinal

    let START = State.make "S"
    let ERROR = State.make "Error"

    type private NFAState = {
        /// The current state from which transitions will be created.
        current: State

        /// An infinite sequence of states.
        states: seq<State>

        /// The input tokens to compile.
        input: Node list

        /// The result tokens to compile.
        result: Node list

        /// Accumulates key-value pairs that will create the transition table.
        transitions: Transition<State> list

        /// Accumulates transformations produced by matching transitions.
        transformations: (Transition<State> * string) list

        /// Specifies the input segment being compiled (environment or placeholder).
        inputNode: InputNode

        /// Specifies the position in the input tokens list (initial or non-initial).
        inputPosition: InputPosition

        /// Specifies the position in the subtree being built (final or non-final).
        subtreePosition: SubtreePosition list
    }

    type private RuleGeneratorState =
        | HasNext of NFAState
        | Done

    let private buildStateMachine (features: Map<string, Node>) sets target result environment showNfa =
        let target = Node.untagAll target
        let result = Node.untagAll result
        let environment = Node.untagAll environment

        let takeState (states: State seq) =
            Seq.tail states, Seq.head states

        let featureTransformations =
            features
            |> Seq.map (fun kvp -> kvp.Key, Node.getTransformations kvp.Value)
            |> Map.ofSeq

        /// <param name="state">The internal NFA state.</param>
        let rec buildStateMachine' state : NFAState =
            let state = {
                state with
                    subtreePosition =
                        match state.subtreePosition with
                        | _::rest when List.length state.input = 1 -> SubtreeFinal :: rest
                        | _::rest -> SubtreeNonfinal :: rest
                        | [] -> failwith "Subtree position stack empty" }

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
                match state.inputNode, result with
                | Environment, _
                | Placeholder, [] ->
                    None, result
                | Placeholder, node::xs ->
                    Some node, xs

            let endCata fEnd fNotEnd fNodeComplete =
                match state.subtreePosition with
                | SubtreeFinal::_ when fNodeComplete() -> fEnd()
                | _ -> fNotEnd()

            let areAllSubtreesFinal () = not (List.contains SubtreeNonfinal state.subtreePosition)

            let getNextState isNodeComplete { states = states } =
                let states, nextState = takeState states
                let nextState =
                    endCata
                        (fun _ -> State.makeFinal nextState)
                        (fun _ -> nextState)
                        (fun () -> isNodeComplete)
                let nextState =
                    match state.inputNode with
                    | Environment -> State.makeEnvironment nextState
                    | _ -> nextState
                { state with states = states }, nextState

            /// Creates a transition to a new state that matches an input symbol.
            let matchCharacter c =
                let state, target = getNextState (areAllSubtreesFinal ()) state
                let transitions = (From state.current, OnChar c, To target) :: state.transitions
                { state with
                    transitions = transitions
                    current = target
                }

            /// <summary>
            /// If possible, adds a transformation for an utterance.
            /// </summary>
            /// <param name="transformations">The accumulated transformations so far.</param>
            /// <param name="t">The transition to producing the transformation.</param>
            /// <param nmae="maybeResult">The next node from the result segment. If none is available, then its value should be None.</param>
            /// <param name="utterance">The utterance to transform.</param>
            let addUtteranceTransformation transformations t maybeResult utterance =
                match maybeResult with
                // replace with another utterance
                | Some (UtteranceNode s) ->
                    (t, s) :: transformations

                // transform by flipping one feature
                | Some (CompoundSetIdentifierNode [FeatureIdentifierNode (isPresent, name)]) ->
                    let additions, removals = featureTransformations.[name]
                    let searchMap = if isPresent then additions else removals

                    match Map.tryFind utterance searchMap with
                    | Some result -> (t, result) :: transformations
                    | None -> transformations

                | _ ->
                    transformations

            /// Creates a series of states and transitions that match each character of an utterance.
            let matchUtterance utterance =
                let rec matchUtterance' input result innerState =
                    match input with
                    | [] ->
                        let resultNode, result = giveResult result
                        let transformations = addUtteranceTransformation innerState.transformations (List.head innerState.transitions) resultNode utterance
                        { innerState with
                            result = result
                            transformations = transformations }

                    | c::xs ->
                        let nextState, target = getNextState (List.isEmpty xs && areAllSubtreesFinal ()) innerState
                        let transitions = (From innerState.current, OnChar c, To target) :: innerState.transitions
                        matchUtterance' xs result
                            { nextState with
                                transitions = transitions
                                current = target }

                matchUtterance' (List.ofSeq utterance) result state

            let addSetTransformation value transition transformations result =
                let resultNode, result = giveResult result

                let transformations =
                    match resultNode with
                    | Some (UtteranceNode utterance) ->
                        (transition, utterance) :: transformations

                    | Some (CompoundSetIdentifierNode (FeatureIdentifierNode (isPresent, name)::[])) ->
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
            /// If any categories specify transformations that match the result of the rule,
            /// those will be added to the transformations list as well.
            let matchSet setDesc =
                let state, terminator = getNextState (areAllSubtreesFinal ()) state

                let rec matchSet' state tree =
                    match tree with
                    | PrefixTree.Root children
                    | PrefixTree.Node (_, _, children) ->
                        // Create states for each child and transitions to them
                        let nextState =
                            (state, children)
                            ||> List.fold
                                (fun innerState n ->
                                    match n with
                                    | PrefixTree.Node (_, c, _) ->
                                        // Create a node for this input symbol and a transition to it, and visit the children.
                                        let states, nextState = takeState innerState.states
                                        let transitions = (From state.current, OnChar c, To nextState) :: innerState.transitions
                                        let nextInnerState = {
                                            innerState with
                                                states = states
                                                transitions = transitions
                                                current = nextState
                                        }
                                        let nextInnerState = matchSet' nextInnerState n
                                        { innerState with
                                            states = nextInnerState.states
                                            transitions = nextInnerState.transitions
                                            transformations = nextInnerState.transformations }

                                    | PrefixTree.Leaf value ->
                                        // No more input symbols. Create a transition to the terminator state.
                                        let t = From state.current, OnEpsilon, To terminator
                                        let nextTransformations, nextResult = addSetTransformation value t innerState.transformations innerState.result
                                        let nextTransitions = t :: innerState.transitions
                                        { innerState with
                                            result = nextResult
                                            transitions = nextTransitions
                                            transformations = nextTransformations }

                                    | PrefixTree.Root _ ->
                                        failwith "A Root should never be the descendant of another node")

                        { nextState with current = terminator }

                    | PrefixTree.Leaf _ ->
                        state

                setDesc
                |> PrefixTree.fromSetIntersection features sets
                |> matchSet' state

            let matchOptional children =
                let state, lastState = getNextState (areAllSubtreesFinal ()) state
                let nextState = buildStateMachine' {
                    state with
                        input = children
                        inputPosition = InputNoninitial
                        subtreePosition = SubtreeNonfinal :: state.subtreePosition
                }
                let transitions =
                    [ From state.current, OnEpsilon, To lastState
                      From nextState.current, OnEpsilon, To lastState ]
                    @ nextState.transitions
                { nextState with
                    transitions = transitions
                    current = lastState
                    subtreePosition = state.subtreePosition }

            let matchDisjunct branches =
                // Create a common exit point for all subtrees
                let state, lastState = getNextState (areAllSubtreesFinal ()) state

                // Build subtree for each branch
                let out =
                    List.foldBack
                        (fun branch ((states, result, transitions, transformations, innerCurrentState)::_ as acc) ->
                            let innerSubtreePosition =
                                match state.subtreePosition with
                                | SubtreeFinal::_ when areAllSubtreesFinal () -> SubtreeFinal
                                | _ -> SubtreeNonfinal
                            let { current = zzzz; states = states; result = result; transitions = transitions; transformations = transformations } =
                                buildStateMachine'
                                    { state with
                                        current = state.current
                                        states = states
                                        input = branch
                                        result = result
                                        transitions = transitions
                                        transformations = transformations
                                        subtreePosition = innerSubtreePosition :: state.subtreePosition }
                            (states, result, transitions, transformations, zzzz) :: acc)
                        branches
                        [state.states, state.result, state.transitions, state.transformations, state.current]

                // Continue with the state of the last branch built.
                let (states, result, transitions, transformations, _)::_ = out

                // Transition from last state of each subtree to lastState.
                // Reverse the list and take the tail first so we don't epsilon from current to lastState and match without consuming input.
                let subtreeFinalToLastState =
                    out
                    |> List.rev
                    |> List.tail
                    |> List.map (fun (_, _, _, _, subtreeFinal) -> From subtreeFinal, OnEpsilon, To lastState)

                let transitions =
                    [ From state.current, OnAny, To ERROR ] @ // Go to ERROR if we can't match anything
                    subtreeFinalToLastState @
                    transitions

                { state with
                    current = lastState
                    states = states
                    result = result
                    transitions = transitions
                    transformations = transformations }

            let matchBoundary () =
                let boundaryChar =
                    match state.inputPosition with
                    | InputInitial -> Special.START
                    | InputNoninitial -> Special.END
                matchCharacter boundaryChar

            let matchTarget () =
                let subtreePosition =
                    match areAllSubtreesFinal () with
                    | true -> SubtreeFinal
                    | false -> SubtreeNonfinal
                let nextState =
                    buildStateMachine'
                        { state with
                            input = target
                            inputNode = Placeholder
                            inputPosition = InputNoninitial
                            subtreePosition = subtreePosition :: state.subtreePosition }
                { nextState with
                    inputNode = Environment
                    subtreePosition = state.subtreePosition }

            let generatorState =
                match state.input with
                | [] ->
                    Done
                | BoundaryNode::_ ->
                    HasNext (matchBoundary ())
                | (UtteranceNode utterance)::_ ->
                    HasNext (matchUtterance utterance)
                | (CompoundSetIdentifierNode setDesc)::_ ->
                    HasNext (matchSet setDesc)
                | (SetIdentifierNode _ as id)::_ ->
                    HasNext (matchSet [ id ])
                | PlaceholderNode::_ ->
                    HasNext (matchTarget ())
                | (OptionalNode children)::_ ->
                    HasNext (matchOptional children)
                | (DisjunctNode branches)::_ ->
                    HasNext (matchDisjunct branches)
                | _ ->
                    failwithf "Unexpected '%s'" (string state.input.Head)

            match generatorState with
            | Done ->
                state
            | HasNext nextState ->
                let input =
                    match state.input, state.subtreePosition with
                    | [_; x], _::SubtreeFinal::_ -> [x]
                    | _::xs, _ -> xs
                buildStateMachine' {
                    nextState with
                        input = input
                        inputPosition = InputNoninitial
                }

        let initialStates = Seq.initInfinite (sprintf "q%d" >> State.make)

        let initialSubtreePosition =
            match environment with
            | [_] -> SubtreeFinal
            | _ -> SubtreeNonfinal

        let initialState = {
            current = START
            states = initialStates
            input = environment
            result = result
            transitions = List.empty
            transformations = List.empty
            inputNode = Environment
            inputPosition = InputInitial
            subtreePosition = [initialSubtreePosition; SubtreeFinal]
        }

        let { transitions = transitions; transformations = transformations } = buildStateMachine' initialState

        let dfa = DeterministicFiniteAutomaton.fromNfa START ERROR (List.rev transitions) transformations showNfa

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

    let compile features sets rule showNfa =
        match Node.untag rule with
        | RuleNode (target, result, environment) ->
            buildStateMachine features sets target result environment showNfa
        | _ ->
            invalidArg "rule" "Must be a RuleNode"
