// Project:     TransmuteLib
// Module:      RuleCompiler
// Description: Rule to finite state transducer converter.
// Copyright:   (c) 2023 Matt Arriola
// License:     MIT

namespace TransmuteLib

open ICSharpCode.SharpZipLib.GZip
open MBrace.FsPickler
open System
open System.IO

module RuleCompiler =
    let internal START = State.make "S"
    let internal ERROR = State.make "Error"

    type CompiledRule = TransitionTable * Map<Transition, TransitionResult>

    type SyllableRule = TransitionTable * Map<Transition, char>

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
        currentNodes: Node list

        /// The tokens from the output section.
        outputNodes: Node list

        /// Accumulates key-value pairs that will create the transition table.
        transitions: Transition list

        /// Accumulates transformations produced by matching transitions.
        transformations: (Transition * TransitionResult) list

        /// Specifies the rule section being compiled (environment or placeholder).
        currentSection: RuleSection

        /// Specifies the position in the input tokens list (initial or non-initial).
        inputPosition: InputPosition

        /// Specifies the position in the subtree being built (final or non-final).
        subtreePositionStack: SubtreePosition list

        /// Indicates whether the placeholder is the next node that will be processed after exiting the subtree
        isPlaceholderNextStack: bool list
    }
    with
        member this.AreAllSubtreesFinal = not (List.contains SubtreeNonfinal this.subtreePositionStack)
        
        member this.IsPlaceholderNext = this.isPlaceholderNextStack |> List.reduce (&&)

    type private RuleGeneratorState =
        | HasNext of NFAState
        | Done

    let private buildNfa showNfa statePrefix (features: Map<string, Node>) sets start input output environment =
        let takeState (states: State seq) =
            Seq.tail states, Seq.head states

        let featureTransformations =
            features
            |> Seq.map (fun kvp -> kvp.Key, Node.getTransformations kvp.Value)
            |> Map.ofSeq

        /// <param name="state">The internal NFA state.</param>
        let rec buildStateMachine' state : NFAState =
            let state =
                { state with
                    subtreePositionStack =
                        match state.subtreePositionStack, state.currentNodes with
                        | _::rest, [_] ->
                            SubtreeFinal :: rest
                        | _::rest, _ ->
                            SubtreeNonfinal :: rest
                        | [], _ ->
                            failwith "Subtree position stack empty"

                    isPlaceholderNextStack =
                        match state.currentNodes with
                        | [ PlaceholderNode _ ] ->
                            false :: state.isPlaceholderNextStack
                        | _ ->
                            let b =
                                state.currentNodes
                                |> Seq.skip (min 1 state.currentNodes.Length)
                                |> Seq.takeWhile (function PlaceholderNode _ -> false | _ -> true)
                                |> Seq.filter (function OptionalNode _ -> false | _ -> true)
                                |> Seq.isEmpty
                            b :: state.isPlaceholderNextStack }

            /// <summary>
            /// When visiting input nodes, and when visiting environment nodes when the input is empty and the last required
            /// node before the placeholder has been consumed, consumes a single symbol and returns a node representing it.
            /// Otherwise consumes nothing and gives nothing.
            /// </summary>
            let giveOutput state =
                match state.currentSection, state.outputNodes, input with
                | InputSection, node::xs, _ ->
                    let nextState = { state with outputNodes = xs }
                    nextState, Some node
                | _, node::xs, [] when state.IsPlaceholderNext ->
                    let nextState = { state with outputNodes = xs }
                    nextState, Some node
                | _ ->
                    state, None

            let getNextState makeFinal state =
                let states, nextState = takeState state.states
                let nextState =
                    if makeFinal && state.AreAllSubtreesFinal
                        then State.makeFinal nextState
                        else nextState

                { state with states = states }, nextState

            /// Creates a transition to a new state that matches an input symbol.
            let matchCharacter c =
                let state, next = getNextState true state
                let transitions = (From state.current, OnChar c, To next) :: state.transitions

                { state with
                    transitions = transitions
                    current = next }

            /// <summary>
            /// Matches one of the boundary characters inserted on either end of the input string, <see cref="Special.START" /> and <see cref="Special.END" />.
            /// </summary>
            let matchBoundary () =
                let boundaryChar =
                    match state.inputPosition with
                    | InputInitial -> Special.START
                    | InputNoninitial -> Special.END
                matchCharacter boundaryChar

            /// Takes a segment, applies a series of transformations to it, and finally adds a transformation to the given transition.
            let addFeatureTransformations transition transformations features depth originalSegment =
                let nextSegment =
                    (originalSegment, features)
                    ||> List.fold (fun segment (FeatureIdentifierNode (isPresent, name)) ->
                        let additions, removals = featureTransformations.[name]
                        let searchMap = if isPresent then additions else removals
                        searchMap
                        |> Map.tryFind segment
                        |> Option.defaultValue segment)

                if nextSegment <> originalSegment then
                    (transition, ReplacesWith (depth, nextSegment)) :: transformations
                else
                    transformations

            /// Creates a series of states and transitions that match each character of an utterance.
            let matchUtterance (utterance: string) =
                // If possible, adds a transformation for an utterance.
                let rec matchUtterance' inputChars innerState =
                    match inputChars with
                    | [] ->
                        let innerState, outputNode = giveOutput innerState
                        let depth = max 0 (utterance.Length - 1)
                        let t = List.head innerState.transitions

                        let transformations =
                            match outputNode with
                            | Some (UtteranceNode s) ->
                                if innerState.IsPlaceholderNext then
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

                        { innerState with transformations = transformations }

                    | c::xs ->
                        let nextNfaState, next = getNextState (xs = []) innerState
                        let transitions = (From innerState.current, OnChar c, To next) :: innerState.transitions

                        matchUtterance' xs
                            { nextNfaState with
                                transitions = transitions
                                current = next }

                matchUtterance' (List.ofSeq utterance) state

            /// Computes the intersection of a list of feature and set identifiers, and creates a
            /// tree of states and transitions that match each member of the resulting set.
            /// If any categories specify transformations that match the output of the rule,
            /// these will be added to the transformation list.
            let matchSet setDesc =
                let state, terminator = getNextState true state

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
                                        let innerState, outputNode = giveOutput innerState

                                        let nextTransformations =
                                            match outputNode with
                                            | Some (UtteranceNode utterance) ->
                                                if innerState.IsPlaceholderNext then
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
                    outputNodes =
                        if state.currentSection = InputSection && nextState.outputNodes.Length > 0 then
                            List.tail nextState.outputNodes
                        else
                            nextState.outputNodes }

            /// Match the input section.
            let matchInput () =
                let subtreePosition = if state.AreAllSubtreesFinal then SubtreeFinal else SubtreeNonfinal
                let nextState =
                    buildStateMachine'
                        { state with
                            currentNodes = input
                            currentSection = InputSection
                            inputPosition = InputNoninitial
                            subtreePositionStack = subtreePosition :: state.subtreePositionStack }
                { nextState with
                    currentSection = EnvironmentSection
                    subtreePositionStack = state.subtreePositionStack }

            /// Optionally match a sequence of nodes. Continue even if no match possible.
            let matchOptional nodes =
                let state, terminator = getNextState true state
                let nextState =
                    buildStateMachine'
                        { state with
                            currentNodes = nodes
                            inputPosition = InputNoninitial
                            subtreePositionStack = SubtreeNonfinal :: state.subtreePositionStack }
                let transitions =
                    [ From state.current, OnEpsilon, To terminator
                      From nextState.current, OnEpsilon, To terminator ]
                    @ nextState.transitions
                { nextState with
                    transitions = transitions
                    current = terminator
                    subtreePositionStack = state.subtreePositionStack }

            let matchDisjunctBranch nodes (branchState::_ as acc) = 
                let nextState =
                    buildStateMachine'
                        { state with
                            current = state.current
                            states = branchState.states
                            currentNodes = nodes
                            transitions = branchState.transitions
                            transformations = branchState.transformations }
                nextState :: acc

            /// Match exactly one of many sequences of nodes.
            let matchDisjunct branches =
                // Create a common exit point for all subtrees
                let state, terminator = getNextState true state

                // Build a subtree for each branch
                let out =
                    List.foldBack
                        matchDisjunctBranch
                        branches
                        [ state ]

                // Continue with the state of the last subtree built.
                let state::_ = out

                // Add epsilon transitions from the last state of each subtree to the terminator state.
                // Reverse the list and take the tail first so we don't epsilon from current to terminator and match without consuming input.
                let subtreeFinalToLastState =
                    out
                    |> Seq.rev
                    |> Seq.tail
                    |> Seq.map (fun state -> From state.current, OnEpsilon, To terminator)
                    |> List.ofSeq

                let insertions =
                    if input = [] then
                        let outputString = output |> Seq.map Node.getStringValue |> String.concat ""
                        subtreeFinalToLastState
                        |> List.map (fun (From subtreeFinal, _, _) -> (From state.current, OnEpsilon, To subtreeFinal), Inserts outputString)
                    else
                        []

                { state with
                    current = terminator
                    outputNodes = output
                    transitions = subtreeFinalToLastState @ state.transitions
                    transformations = state.transformations @ insertions }

            let generatorState =
                match state.currentNodes with
                | [] ->
                    Done
                | WordBoundaryNode::_ ->
                    HasNext (matchBoundary ())
                | (UtteranceNode utterance)::_ ->
                    HasNext (matchUtterance utterance)
                | (CompoundSetIdentifierNode setDesc)::_  ->
                    HasNext (matchSet setDesc)
                | (SetIdentifierNode _ as id)::_ ->
                    HasNext (matchSet [ id ])
                | PlaceholderNode::_ ->
                    HasNext (matchInput ())
                | (OptionalNode children)::_ ->
                    HasNext (matchOptional children)
                | (DisjunctNode branches)::_ ->
                    HasNext (matchDisjunct branches)
                | x::_ ->
                    failwithf $"Unexpected {x}"

            match generatorState with
            | Done ->
                state
            | HasNext nextState ->
                buildStateMachine'
                    { nextState with
                        isPlaceholderNextStack = List.tail nextState.isPlaceholderNextStack
                        currentNodes = List.tail state.currentNodes
                        inputPosition = InputNoninitial }

        let initialState = {
            current = start
            states = Seq.initInfinite (string >> (+) statePrefix >> State.make)
            currentNodes = environment
            outputNodes = output
            transitions = []
            transformations = []
            currentSection = EnvironmentSection
            inputPosition = InputInitial
            subtreePositionStack = [ if List.isEmpty environment then SubtreeNonfinal else SubtreeFinal ]
            isPlaceholderNextStack = []
        }

        let { transitions = transitions; transformations = transformations } = buildStateMachine' initialState

        transitions, transformations

    let joinNfas sOnset sNucleus sCoda (nfas: Map<State * InputSymbol, State> list) =
        let transitions = nfas |> List.collect (Map.toList)

        let finalStates =
            nfas
            |> List.map (fun table ->
                table
                |> Map.toList
                |> List.choose (fun (_, dest) -> if State.isFinal dest then Some dest else None)
                |> List.distinct)
            |> List.ofSeq
        
        let onsetToNucleus = finalStates[0] |> List.map (fun s -> (s, OnEpsilon), sNucleus)
        let nucleusToCoda = finalStates[1] |> List.map (fun s -> (s, OnEpsilon), sCoda)
        let codaToOnset = finalStates[2] |> List.map (fun s -> (s, OnEpsilon), sOnset)
        
        let transitions = transitions @ onsetToNucleus @ nucleusToCoda |> Map.ofList

        let onsetTransformations = nfas[0] |> Map.toList |> List.map (fun ((origin, input), dest) -> (From origin, input, To dest), 'O')
        let nucleusTransformations = nfas[1] |> Map.toList |> List.map (fun ((origin, input), dest) -> (From origin, input, To dest), 'N')
        let codaTransformations = nfas[2] |> Map.toList |> List.map (fun ((origin, input), dest) -> (From origin, input, To dest), 'C')
        let transformations = Map.ofList (onsetTransformations @ nucleusTransformations @ codaTransformations)

        transitions, transformations

    let keepAsNfa showNfa (transitions, transformations) =
        let dfaTransitionTable =
            transitions
            |> Seq.map (fun (From origin, input, To dest) -> (origin, input), dest)
            |> Map.ofSeq

        let dfaTransformations =
            transformations
            |> Seq.choose (function
                | _, OutputDefault -> None
                | t, output -> Some (t, output))
            |> Map.ofSeq

        dfaTransitionTable, dfaTransformations

    let toDfa showNfa start (transitions, transformations) =
        let dfa = DeterministicFiniteAutomaton.fromNfa start ERROR (List.rev transitions) transformations showNfa

        let dfaTransitionTable =
            dfa
            |> Seq.map (fun ((From origin, input, To dest), _) -> (origin, input), dest)
            |> Map.ofSeq

        let dfaTransformations =
            dfa
            |> Seq.choose (function
                | _, OutputDefault -> None
                | t, output -> Some (t, output))
            |> Map.ofSeq

        dfaTransitionTable, dfaTransformations

    /// <summary>
    /// Compiles a finite state transducer from a phonological rule.
    /// The resulting state machine can be passed into <see cref="RuleMachine.transform" /> to apply the rule to a word.
    /// </summary>
    let compileRule showNfa features sets rule : CompiledRule =
        match Node.untag rule with
        | RuleNode (input, output, environment) ->
            let input = Node.untagAll input
            let output = Node.untagAll output
            let environment = Node.untagAll environment
            buildNfa showNfa "q" features sets START input output environment
            |> toDfa showNfa START
        | _ ->
            invalidArg "rule" "Must be a RuleNode"

    let toList (transitions, transformations) =
        let transitions =
            transitions
            |> Seq.map (fun (From origin, input, To dest) -> (origin, input), dest)
            |> Map.ofSeq
        transitions, transformations

    /// Compiles a finite state transducer from a set of syllable definition rules.
    let compileSyllableRule showNfa features sets rule : SyllableRule =
        match rule with
        | SyllableDefinitionNode (onset, nucleus, coda) ->
            let S_ONSET = State.make "S_onset"
            let S_NUCLEUS = State.make "S_nucleus"
            let S_CODA = State.make "S_coda"
            let onset, _ = Node.untagAll onset |> buildNfa showNfa "o" features sets S_ONSET [] [] |> toList
            let nucleus, _ = Node.untagAll nucleus |> buildNfa showNfa "n" features sets S_NUCLEUS [] [] |> toList
            let coda, _ = Node.untagAll coda |> buildNfa showNfa "c" features sets S_CODA [] [] |> toList
            [ onset; nucleus; coda ]
            |> joinNfas S_ONSET S_NUCLEUS S_CODA
        | _ ->
            invalidArg "rule" "Must be a SyllableDefinitionNode"

    let compileRules showNfa features sets rules =
        rules
        |> List.map (fun rule -> compileRule showNfa features sets rule)

    let compileRulesParallel showNfa features sets rules =
        rules
        |> Array.ofList
#if DEBUG
        |> Array.mapi (fun i rule -> compileRule showNfa features sets rule)
#else
        |> Array.Parallel.mapi (fun i rule -> compileRule showNfa features sets rule)
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
