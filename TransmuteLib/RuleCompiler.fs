// TODO: cleanup (make nested functions module-level and private)

namespace TransmuteLib
open MBrace.FsPickler
open System
open System.IO
open Joveler.Compression.XZ
open Microsoft.FSharp.Collections

module RuleCompiler =
    let internal START = State.make "S"
    let internal ERROR = State.make "Error"

    type private InputPosition =
        | InputInitial
        | InputNoninitial

    type private InputNode =
        | Environment
        | Placeholder

    type private SubtreePosition =
        | SubtreeNonfinal
        | SubtreeFinal

    type private NFAState = {
        /// The current state from which transitions will be created.
        current: State

        /// An infinite sequence of states.
        states: State seq

        /// The tokens from the segment currently being processed.
        input: Node list

        /// The tokens from the output segment.
        output: Node list

        /// Accumulates key-value pairs that will create the transition table.
        transitions: Transition<State> list

        /// Accumulates transformations produced by matching transitions.
        transformations: (Transition<State> * string) list

        /// Specifies the rule segment being compiled (environment or placeholder).
        inputNode: InputNode

        /// Specifies the position in the input tokens list (initial or non-initial).
        inputPosition: InputPosition

        /// Specifies the position in the subtree being built (final or non-final).
        subtreePosition: SubtreePosition list
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
            /// When visiting output nodes, consumes a single symbol and returns a node representing it.
            /// When visiting environment nodes or when there is no output left to give, consumes nothing and gives nothing.
            /// </summary>
            /// <remarks>
            /// Utterance nodes are consumed one symbol at a time by returning an UtteranceNode containing the first
            /// character in the utterance, and replacing the head output node with an UtteranceNode containing the
            /// remainder. The only other nodes allowed in the output section, SetIdentifierNode and
            /// CompoundSetIdentifierNode, are taken from the output whole.
            /// </remarks>
            let giveOutput output =
                match state.inputNode, output with
                | Environment, _
                | Placeholder, [] ->
                    None, output
                | Placeholder, node::xs ->
                    Some node, xs

            let areAllSubtreesFinal () = not (List.contains SubtreeNonfinal state.subtreePosition)

            let getNextState isFinal { states = states } =
                let states, nextState = takeState states
                let nextState =
                    if isFinal
                        then State.makeFinal nextState
                        else nextState
                let nextState =
                    match state.inputNode with
                    | Environment -> State.makeEnvironment nextState
                    | _ -> nextState
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

            /// <summary>
            /// If possible, adds a transformation for an utterance.
            /// </summary>
            /// <param name="transformations">The accumulated transformations so far.</param>
            /// <param name="t">The transition producing the transformation.</param>
            /// <param nmae="maybeOutput">The next node from the output segment. If none is available, then its value should be None.</param>
            /// <param name="utterance">The utterance to transform.</param>
            let addUtteranceTransformation transformations t maybeOutput utterance =
                match maybeOutput with
                // replace with another utterance
                | Some (UtteranceNode s) ->
                    (t, s) :: transformations

                // transform by flipping one feature
                | Some (CompoundSetIdentifierNode [FeatureIdentifierNode (isPresent, name)]) ->
                    let additions, removals = featureTransformations.[name]
                    let searchMap = if isPresent then additions else removals

                    match Map.tryFind utterance searchMap with
                    | Some output -> (t, output) :: transformations
                    | None -> transformations

                | _ ->
                    transformations

            /// Creates a series of states and transitions that match each character of an utterance.
            let matchUtterance utterance =
                let rec matchUtterance' inputChars output innerState =
                    match inputChars with
                    | [] ->
                        let outputNode, output = giveOutput output
                        let transformations = addUtteranceTransformation innerState.transformations (List.head innerState.transitions) outputNode utterance

                        { innerState with
                            output = output
                            transformations = transformations }

                    | c::xs ->
                        let nextNfaState, next = getNextState (List.isEmpty xs && areAllSubtreesFinal ()) innerState
                        let next = if input = [] then State.makeInsert next else next
                        let transitions = (From innerState.current, OnChar c, To next) :: innerState.transitions
                        //let transitionsToCurrent = List.filter (fun ((From _, _, To s) as t) -> s = innerState.current) transitions
                        let insertion =
                            if input = [] then
                                let outputString = output |> List.map Node.getStringValue |> String.concat ""
                                [ (From state.current, OnChar c, To next), outputString ]
                                //List.map (fun t -> t, outputString) transitionsToCurrent
                            else
                                []

                        matchUtterance' xs output
                            { nextNfaState with
                                transitions = transitions
                                transformations = state.transformations @ insertion
                                current = next }

                matchUtterance' (List.ofSeq utterance) output state

            let addSetTransformation value transition transformations output =
                let outputNode, output = giveOutput output

                let transformations =
                    match outputNode with
                    | Some (UtteranceNode utterance) ->
                        (transition, utterance) :: transformations

                    | Some (CompoundSetIdentifierNode (FeatureIdentifierNode (isPresent, name)::[])) ->
                        let additions, removals = featureTransformations.[name]
                        let searchMap = if isPresent then additions else removals
                        match Map.tryFind value searchMap with
                        | Some output ->
                            (transition, output) :: transformations
                        | None ->
                            // If no transformation is defined for this sound, just "transform" it to itself.
                            (transition, value) :: transformations

                    | Some (CompoundSetIdentifierNode _) ->
                        failwith "Only one feature may be transformed at a time"

                    | _ ->
                        transformations

                transformations, output

            /// Computes the intersection of a list of feature and set identifiers, and creates a
            /// tree of states and transitions that match each member of the resulting set.
            /// If any categories specify transformations that match the output of the rule,
            /// these will be added to the transformation list.
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
                                        // We have reached a leaf node and now have a complete sound to transform.
                                        // Add the transformation and a transition to the terminator state.
                                        let t = From state.current, OnEpsilon, To terminator
                                        let nextTransformations, nextOutput = addSetTransformation value t innerState.transformations innerState.output
                                        let nextTransitions = t :: innerState.transitions
                                        { innerState with
                                            output = nextOutput
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

            /// Match the input segment.
            let matchInput () =
                let subtreePosition =
                    match areAllSubtreesFinal () with
                    | true -> SubtreeFinal
                    | false -> SubtreeNonfinal
                let nextState =
                    buildStateMachine'
                        { state with
                            input = input
                            inputNode = Placeholder
                            inputPosition = InputNoninitial
                            subtreePosition = subtreePosition :: state.subtreePosition }
                { nextState with
                    inputNode = Environment
                    subtreePosition = state.subtreePosition }

            /// Optionally match a sequence of nodes. Continue even if no match possible.
            let matchOptional nodes =
                let state, terminator = getNextState (areAllSubtreesFinal ()) state
                let nextState = buildStateMachine' {
                    state with
                        input = nodes
                        inputPosition = InputNoninitial
                        subtreePosition = SubtreeNonfinal :: state.subtreePosition
                }
                let transitions =
                    [ From state.current, OnEpsilon, To terminator
                      From nextState.current, OnEpsilon, To terminator ]
                    @ nextState.transitions
                { nextState with
                    transitions = transitions
                    current = terminator
                    subtreePosition = state.subtreePosition }

            let matchDisjunctBranch nodes ((states, output, transitions, transformations, _)::_ as acc) = 
                let innerSubtreePosition =
                    match state.subtreePosition with
                    | SubtreeFinal::_ when areAllSubtreesFinal () -> SubtreeFinal
                    | _ -> SubtreeNonfinal
                let { current = current; states = states; output = output; transitions = transitions; transformations = transformations } =
                    buildStateMachine'
                        { state with
                            current = state.current
                            states = states
                            input = nodes
                            output = output
                            transitions = transitions
                            transformations = transformations
                            subtreePosition = innerSubtreePosition :: state.subtreePosition }
                (states, output, transitions, transformations, current) :: acc

            /// Match exactly one of many sequences of nodes.
            let matchDisjunct branches =
                // Create a common exit point for all subtrees
                let state, terminator = getNextState (areAllSubtreesFinal ()) state

                // Build a subtree for each branch
                let out =
                    List.foldBack
                        matchDisjunctBranch
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
                        |> List.map (fun (From subtreeFinal, _, _) -> (From state.current, OnEpsilon, To subtreeFinal), outputString)
                    else
                        []

                { state with
                    current = terminator
                    states = states
                    output = output
                    transitions = subtreeFinalToLastState @ transitions
                    transformations = transformations @ insertions }

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
                    HasNext (matchInput ())
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
            inputNode = Environment
            inputPosition = InputInitial
            subtreePosition = [if List.isEmpty environment then SubtreeNonfinal else SubtreeFinal]
        }

        let { transitions = transitions; transformations = transformations } = buildStateMachine' initialState

        let dfa = DeterministicFiniteAutomaton.fromNfa START ERROR (List.rev transitions) transformations showNfa

        let dfaTransformations =
            dfa
            |> List.choose (function
                | t, Produces output -> Some (t, output)
                | _, NoOutput -> None)
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
    let compile showNfa features sets rule =
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
        |> PSeq.mapi (fun i rule -> compile showNfa features sets rule)
        |> PSeq.toList
        //|> List.sortBy fst
        //|> List.map snd

    let saveCompiledRules filename rules =
        Utils.Lzma.init() |> ignore
        use f = File.Open(filename, FileMode.Create, FileAccess.Write)
        use xz = new XZStream(f, new XZCompressOptions())

        let serializer = FsPickler.CreateBinarySerializer()
        let pickle = serializer.Pickle(rules)

        xz.Write(BitConverter.GetBytes(pickle.Length), 0, 4)
        xz.Write(pickle, 0, pickle.Length)

    let readCompiledRules filename =
        Utils.Lzma.init() |> ignore
        use f = File.Open(filename, FileMode.Open, FileAccess.Read)
        use xz = new XZStream(f, new XZDecompressOptions())

        let lengthBuffer: byte[] = Array.zeroCreate 4
        xz.Read(lengthBuffer, 0, 4) |> ignore
        let length = BitConverter.ToInt32(lengthBuffer, 0)

        let buffer: byte[] = Array.zeroCreate length
        xz.Read(buffer, 0, length) |> ignore

        let serializer = FsPickler.CreateBinarySerializer()
        serializer.UnPickle(buffer)
