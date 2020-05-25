// TODO: cleanup (make nested functions module-level and private)

namespace TransmuteLib

open Node
open StateMachine
open System

module SoundChangeRule =
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

    let private START = State.make "S"
    let private ERROR = State.make "Error"

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

    let private buildStateMachine (features: Map<string, Node>) sets target result environment showNfa =
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
        match untag rule with
        | RuleNode (target, result, environment) ->
            buildStateMachine features sets target result environment showNfa
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
