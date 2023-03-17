namespace TransmuteLib

open ICSharpCode.SharpZipLib.GZip
open MBrace.FsPickler
open System
open System.IO

module RuleCompiler =
    let internal START = State.make "S"
    let internal ERROR = State.make "Error"

    type CompiledRule = TransitionTable<State> * Map<Transition<State>, TransitionResult>

    type private InputPosition =
        | InputInitial
        | InputNoninitial

    type private SubtreePosition =
        | SubtreeNonfinal
        | SubtreeFinal

    type private RuleSection =
        | InputSection
        | EnvironmentSection

    type private NFAState = {
        /// The current state from which transitions will be created.
        current: State

        /// An infinite sequence of states.
        states: State seq

        /// The tokens from the section currently being processed.
        input: Node list

        /// The tokens from the output section.
        output: Node list

        /// Accumulates key-value pairs that will create the transition table.
        transitions: Transition<State> list

        /// Accumulates transformations produced by matching transitions.
        transformations: (Transition<State> * TransitionResult) list

        /// Specifies the rule section being compiled (environment or placeholder).
        currentSection: RuleSection

        /// Specifies the position in the input tokens list (initial or non-initial).
        inputPosition: InputPosition

        /// Specifies the position in the subtree being built (final or non-final).
        subtreePosition: SubtreePosition list

        /// Indicates whether the placeholder is the next node that will be processed after exiting the subtree
        isPlaceholderNext: bool
    }

    type private RuleGeneratorState =
        | HasNext of NFAState
        | Done

    let private buildStateMachine (features: Map<string, Node>) sets input output environment showNfa =
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
            /// When visiting input nodes, and when visiting environment nodes when the input is empty and the last required
            /// node before the placeholder has been consumed, consumes a single symbol and returns a node representing it.
            /// Otherwise consumes nothing and gives nothing.
            /// </summary>
            let giveOutput isPlaceholderNext output =
                match state.currentSection, output, input with
                | InputSection, node::xs, _ ->
                    xs, Some node
                | _, node::xs, [] when isPlaceholderNext ->
                    xs, Some node
                | _ ->
                    output, None

            let areAllSubtreesFinal () = not (List.contains SubtreeNonfinal state.subtreePosition)

            let getNextState isFinal { states = states } =
                let states, nextState = takeState states
                let nextState =
                    if isFinal
                        then State.makeFinal nextState
                        else nextState
                { state with states = states }, nextState

            /// Creates a transition to a new state that matches an input symbol.
            let matchCharacter c =
                let state, next = getNextState (areAllSubtreesFinal ()) state
                let transitions = (From state.current, OnChar c, To next) :: state.transitions
                { state with
                    transitions = transitions
                    current = next
                }

            /// <summary>
            /// Matches one of the boundary characters inserted on either end of the input string, <see cref="Special.START" /> and <see cref="Special.END" />.
            /// </summary>
            let matchBoundary () =
                let boundaryChar =
                    match state.inputPosition with
                    | InputInitial -> Special.START
                    | InputNoninitial -> Special.END
                matchCharacter boundaryChar

            let hasNoMoreRequiredNodes xs =
                if state.currentSection = InputSection then
                    false
                else
                    xs
                    |> List.takeWhile (function PlaceholderNode _ -> false | _ -> true)
                    |> List.exists (function OptionalNode _ -> false | _ -> true)
                    |> not

            let addFeatureTransformations transition transformations features depth originalValue =
                let rec addFeatureTransformations' transformations features value =
                    match features with
                    | [] ->
                        if value <> originalValue then
                            (transition, ReplacesWith (depth, value)) :: transformations
                        else
                            transformations

                    | FeatureIdentifierNode (isPresent, name)::rest ->
                        let additions, removals = featureTransformations.[name]
                        let searchMap = if isPresent then additions else removals

                        let nextValue =
                            match Map.tryFind value searchMap with
                            | Some output -> output
                            | None -> value

                        addFeatureTransformations' transformations rest nextValue

                addFeatureTransformations' transformations features originalValue

            /// Creates a series of states and transitions that match each character of an utterance.
            let matchUtterance rest isPlaceholderNext (utterance: string) =
                // If possible, adds a transformation for an utterance.

                let rec matchUtterance' inputChars output innerState =
                    match inputChars with
                    | [] ->
                        let output, outputNode = giveOutput (isPlaceholderNext || innerState.isPlaceholderNext) output
                        let depth = max 0 (utterance.Length - 1)
                        let t = List.head innerState.transitions

                        let transformations =
                            match outputNode with
                            | Some (UtteranceNode s) ->
                                let shouldInsert = hasNoMoreRequiredNodes rest && (isPlaceholderNext || state.isPlaceholderNext)

                                if shouldInsert then
                                    (t, Inserts s) :: innerState.transformations
                                elif state.currentSection = InputSection then
                                    (t, ReplacesWith (depth, s)) :: innerState.transformations
                                else
                                    innerState.transformations

                            | Some (CompoundSetIdentifierNode features) ->
                                addFeatureTransformations t innerState.transformations features depth utterance

                            | _ ->
                                if state.currentSection = InputSection then
                                    (t, Deletes (utterance.Length, utterance)) :: innerState.transformations
                                else
                                    innerState.transformations

                        { innerState with
                            output = output
                            transformations = transformations }

                    | c::xs ->
                        let nextNfaState, next = getNextState (List.isEmpty xs && areAllSubtreesFinal ()) innerState
                        let transitions = (From innerState.current, OnChar c, To next) :: innerState.transitions

                        matchUtterance' xs output
                            { nextNfaState with
                                transitions = transitions
                                current = next }

                matchUtterance' (List.ofSeq utterance) state.output state

            /// Computes the intersection of a list of feature and set identifiers, and creates a
            /// tree of states and transitions that match each member of the resulting set.
            /// If any categories specify transformations that match the output of the rule,
            /// these will be added to the transformation list.
            let matchSet rest isPlaceholderNext setDesc =
                let state, terminator = getNextState (areAllSubtreesFinal ()) state

                let rec matchSet' state tree =
                    match tree with
                    | PrefixTree.Leaf _ ->
                        state

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

                                    | PrefixTree.Leaf (value, depth) ->
                                        // We have reached a leaf node and now have a complete segment to transform.
                                        // Add the transformation and a transition to the terminator state.
                                        let t = From state.current, OnEpsilon, To terminator
                                        let nextOutput, outputNode = giveOutput (isPlaceholderNext || state.isPlaceholderNext) innerState.output

                                        let nextTransformations =
                                            match outputNode with
                                            | Some (UtteranceNode utterance) ->
                                                let shouldInsert = hasNoMoreRequiredNodes rest && (isPlaceholderNext || state.isPlaceholderNext)

                                                if shouldInsert then
                                                    (t, Inserts utterance) :: innerState.transformations
                                                else
                                                    (t, ReplacesWith (depth, utterance)) :: innerState.transformations

                                            | Some (CompoundSetIdentifierNode features) ->
                                                addFeatureTransformations t innerState.transformations features depth value

                                            | _ ->
                                                if state.currentSection = InputSection then
                                                    (t, Deletes (value.Length, value)) :: innerState.transformations
                                                else
                                                    innerState.transformations

                                        { innerState with
                                            output = nextOutput
                                            transitions = t :: innerState.transitions
                                            transformations = nextTransformations }

                                    | PrefixTree.Root _ ->
                                        failwith "A Root should never be the descendant of another node")

                        { nextState with current = terminator }

                let nextState =
                    setDesc
                    |> PrefixTree.fromSetIntersection features sets
                    |> matchSet' state

                { nextState with
                    output =
                        if state.currentSection = InputSection then
                            nextState.output[1..]
                        else
                            nextState.output }

            /// Match the input section.
            let matchInput () =
                let subtreePosition =
                    match areAllSubtreesFinal () with
                    | true -> SubtreeFinal
                    | false -> SubtreeNonfinal
                let nextState =
                    buildStateMachine'
                        { state with
                            input = input
                            currentSection = InputSection
                            inputPosition = InputNoninitial
                            subtreePosition = subtreePosition :: state.subtreePosition }
                { nextState with
                    currentSection = EnvironmentSection
                    subtreePosition = state.subtreePosition }

            /// Optionally match a sequence of nodes. Continue even if no match possible.
            let matchOptional rest isPlaceholderNext nodes =
                let state, terminator = getNextState (areAllSubtreesFinal ()) state
                let nextState = buildStateMachine' {
                    state with
                        input = nodes
                        inputPosition = InputNoninitial
                        subtreePosition = SubtreeNonfinal :: state.subtreePosition
                        isPlaceholderNext = isPlaceholderNext
                }
                let transitions =
                    [ From state.current, OnEpsilon, To terminator
                      From nextState.current, OnEpsilon, To terminator ]
                    @ nextState.transitions
                { nextState with
                    transitions = transitions
                    current = terminator
                    subtreePosition = state.subtreePosition }

            let matchDisjunctBranch isPlaceholderNext nodes ((states, output, transitions, transformations, _)::_ as acc) = 
                let innerSubtreePosition =
                    match state.subtreePosition with
                    | SubtreeFinal::_ when areAllSubtreesFinal () -> SubtreeFinal
                    | _ -> SubtreeNonfinal
                let { current = current; states = states; transitions = transitions; transformations = transformations } =
                    buildStateMachine'
                        { state with
                            current = state.current
                            states = states
                            input = nodes
                            transitions = transitions
                            transformations = transformations
                            subtreePosition = innerSubtreePosition :: state.subtreePosition
                            isPlaceholderNext = isPlaceholderNext }
                (states, output, transitions, transformations, current) :: acc

            /// Match exactly one of many sequences of nodes.
            let matchDisjunct rest isPlaceholderNext branches =
                // Create a common exit point for all subtrees
                let state, terminator = getNextState (areAllSubtreesFinal ()) state

                // Build a subtree for each branch
                let out =
                    List.foldBack
                        (matchDisjunctBranch isPlaceholderNext)
                        branches
                        [state.states, state.output, state.transitions, state.transformations, state.current]

                // Continue with the state of the last subtree built.
                let (states, output, transitions, transformations, _)::_ = out

                // Add epsilon transitions from the last state of each subtree to the terminator state.
                // Reverse the list and take the tail first so we don't epsilon from current to terminator and match without consuming input.
                let subtreeFinalToLastState =
                    out
                    |> List.rev
                    |> List.tail
                    |> List.map (fun (_, _, _, _, subtreeFinal) -> From subtreeFinal, OnEpsilon, To terminator)

                let insertions =
                    if input = [] then
                        let outputString = output |> List.map Node.getStringValue |> String.concat ""
                        subtreeFinalToLastState
                        |> List.map (fun (From subtreeFinal, _, _) -> (From state.current, OnEpsilon, To subtreeFinal), Inserts outputString)
                    else
                        []

                { state with
                    current = terminator
                    states = states
                    output = output
                    transitions = subtreeFinalToLastState @ transitions
                    transformations = transformations @ insertions }

            let generatorState =
                let isPlaceholderNext =
                    match state.input with
                    | [ PlaceholderNode _ ] ->
                        false
                    | _ ->
                        state.input
                        |> Seq.skip (min 1 state.input.Length)
                        |> Seq.takeWhile (function PlaceholderNode _ -> false | _ -> true)
                        |> Seq.filter (function OptionalNode _ -> false | _ -> true)
                        |> Seq.isEmpty
                match state.input with
                | [] ->
                    Done
                | WordBoundaryNode::_ ->
                    HasNext (matchBoundary ())
                | (UtteranceNode utterance)::rest ->
                    HasNext (matchUtterance rest isPlaceholderNext utterance)
                | (CompoundSetIdentifierNode setDesc)::rest  ->
                    HasNext (matchSet rest isPlaceholderNext setDesc)
                | (SetIdentifierNode _ as id)::rest ->
                    HasNext (matchSet rest isPlaceholderNext [ id ])
                | PlaceholderNode::_ ->
                    HasNext (matchInput ())
                | (OptionalNode children)::rest ->
                    HasNext (matchOptional rest isPlaceholderNext children)
                | (DisjunctNode branches)::rest ->
                    HasNext (matchDisjunct rest isPlaceholderNext branches)
                | _ ->
                    failwithf "Unexpected '%s'" (string state.input.Head)

            match generatorState with
            | Done ->
                state
            | HasNext nextState ->
                buildStateMachine' {
                    nextState with
                        input = List.tail state.input
                        inputPosition = InputNoninitial
                }

        let initialState = {
            current = START
            states = Seq.initInfinite (sprintf "q%d" >> State.make)
            input = environment
            output = output
            transitions = List.empty
            transformations = List.empty
            currentSection = EnvironmentSection
            inputPosition = InputInitial
            subtreePosition = [if List.isEmpty environment then SubtreeNonfinal else SubtreeFinal]
            isPlaceholderNext = false
        }

        let { transitions = transitions; transformations = transformations } = buildStateMachine' initialState

        let dfa = DeterministicFiniteAutomaton.fromNfa START ERROR (List.rev transitions) transformations showNfa

        let dfaTransformations =
            dfa
            |> List.choose (function
                | _, OutputDefault -> None
                | t, output -> Some (t, output))
            |> Map.ofList

        let dfaTransitionTable =
            dfa
            |> List.map (fun ((From origin, input, To dest), _) -> (origin, input), dest)
            |> Map.ofList

        dfaTransitionTable, dfaTransformations

    /// <summary>
    /// Compiles a Mealy machine from a phonological rule.
    /// The resulting state machine can be passed into <see cref="RuleMachine.transform" /> to apply the rule to a word.
    /// </summary>
    let compile showNfa features sets rule : CompiledRule =
        match Node.untag rule with
        | RuleNode (input, output, environment) ->
            let input = Node.untagAll input
            let output = Node.untagAll output
            let environment = Node.untagAll environment
            buildStateMachine features sets input output environment showNfa
        | _ ->
            invalidArg "rule" "Must be a RuleNode"

    let compileRules showNfa features sets rules =
        rules
        |> List.map (fun rule -> compile showNfa features sets rule)

    let compileRulesParallel showNfa features sets rules =
        rules
        |> Array.ofList
#if DEBUG
        |> Array.mapi (fun i rule -> compile showNfa features sets rule)
#else
        |> Array.Parallel.mapi (fun i rule -> compile showNfa features sets rule)
#endif
        |> Array.toList

#if !FABLE_COMPILER
    let saveCompiledRules filename rules =
        use f = File.Open(filename, FileMode.Create, FileAccess.Write)
        use gzip = new GZipOutputStream(f)
        gzip.SetLevel 9

        let serializer = FsPickler.CreateBinarySerializer()
        let pickle = serializer.Pickle(rules)

        gzip.Write(BitConverter.GetBytes(pickle.Length), 0, 4)
        gzip.Write(pickle, 0, pickle.Length)

    let readCompiledRules filename =
        use f = File.Open(filename, FileMode.Open, FileAccess.Read)
        use gzip = new GZipInputStream(f)

        let lengthBuffer: byte[] = Array.zeroCreate 4
        gzip.Read(lengthBuffer, 0, 4) |> ignore
        let length = BitConverter.ToInt32(lengthBuffer, 0)

        let buffer: byte[] = Array.zeroCreate length
        gzip.Read(buffer, 0, length) |> ignore

        let serializer = FsPickler.CreateBinarySerializer()
        serializer.UnPickle(buffer)
#endif
