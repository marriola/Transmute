namespace TransmuteLib

open TransmuteLib.StateMachine

module RuleMachine =
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

    /// Returns the original value if input is the special begin or end symbol.
    /// Otherwise, returns the result of fTransform.
    let private specialSymbolCata fTransform input value =
        match input with
        | x when x = Special.START || x = Special.END ->
            value
        | _ ->
            fTransform()

    /// An input symbol tagged with the segment it came from and its position.
    type private EnvironmentString =
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
                    (fun () -> string c :: xs)
                    c xs

            /// Tags the input symbol c with its position and adds it to xs.
            static member addWithPosition c position xs =
                specialSymbolCata
                    (fun () -> (string c, position) :: xs)
                    c xs

            /// Tags the input symbol as originating from the target segment and adds it to xs.
            static member addTarget position c xs =
                specialSymbolCata
                    (fun () -> (FromTarget (string c, position)) :: xs)
                    c xs

            /// Tags the input symbol as originating from the environment segment and adds it to xs.
            static member addEnvironment position c xs =
                specialSymbolCata
                    (fun () -> (FromEnvironment (string c, position)) :: xs)
                    c xs

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

    type private RuleMachineState = {
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

        /// <summary>
        /// The breakpoint.
        /// </summary>
        /// <remarks>
        /// Whenever the state machine encounters an optional node or a disjunct node,
        /// it may fail to match that branch of the rule, but still be able to back up
        /// and visit a different branch. When the state machine is presented with such
        /// a choice, it picks a branch and the remaining options are stored in the
        /// breakpoint property
        /// </remarks>
        breakpoint: Breakpoint list
    }
    and private Breakpoint = {
        input: char[]
        state: RuleMachineState
    }

    /// <summary>
    /// Applies a compiled rule to a word.
    /// </summary>
    /// <param name="verbose">If true, displays the state of the state machine at each step.</param>
    /// <param name="rule">The rule to apply.</param>
    /// <param name="word">The word to transform.</param>
    let transform verbose rule word =
        let transitions, transformations = rule
        stateMachineConfig()
        |> withTransitions transitions
        |> withStartState RuleCompiler.START
        |> withErrorState RuleCompiler.ERROR
        |> withInitialValue {
            isPartialMatch = false
            wasLastFinal = false
            lastOutputOn = None
            production = []
            undo = []
            output = []
            breakpoint = []
        }
        |> onError (fun position input current value _ ->
            let nextOutput =
                match value.isPartialMatch, value.wasLastFinal with
                // The rule failed to match
                | true, false ->
                    EnvironmentString.concatAll value.undo value.output
                // The rule failed to match completely, but what did match was enough to commit the production (i.e. any remaining nodes were optional).
                | true, true ->
                    // undo and production will never be populated at the same time
                    (List.map fst value.production) @ EnvironmentString.concatAll value.undo value.output
                // The rule has not yet begun to match.
                | false, _ ->
                    EnvironmentString.add input value.output
            if verbose then
                printf "  %2d %c %2s: " position input (if value.lastOutputOn = None then "" else string (Option.get value.lastOutputOn))
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
                printf "• %2d %c %2s: " position input (if lastOutputOn = None then "" else string (Option.get lastOutputOn))
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
            |> String.concat "")
        |> runStateMachine (string Special.START + word + string Special.END)
