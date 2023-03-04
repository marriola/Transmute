namespace TransmuteLib

open TransmuteLib.StateMachine

module RuleMachine =
    let private (|IsInput|_|) state =
        match state with
        | State (_, InputSegment, _)
        | MergedState (_, InputSegment) ->
            Some state
        | _ ->
            None

    let private (|IsEnvironment|_|) state =
        match state with
        | State (_, EnvironmentSegment, _)
        | MergedState (_, EnvironmentSegment) ->
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
    type internal EnvironmentString =
        | FromInput of input: string * position: int
        | FromEnvironment of input: string * position: int
        with
            /// Gets the input symbol.
            static member get = function
                | FromEnvironment (s, _)
                | FromInput (s, _) -> s

            /// Adds the input symbol c to xs.
            static member addOutput c xs =
                specialSymbolCata
                    (fun () -> string c :: xs)
                    c xs

            /// Tags the symbol c with its position and adds it to xs.
            static member addProduction c position xs =
                specialSymbolCata
                    (fun () -> (string c, position) :: xs)
                    c xs

            /// Tags the input symbol as originating from the input segment and adds it to xs.
            static member addFromInput position c xs =
                specialSymbolCata
                    (fun () -> (FromInput (string c, position)) :: xs)
                    c xs

            /// Tags the input symbol as originating from the environment segment and adds it to xs.
            static member addFromEnvironment position c xs =
                specialSymbolCata
                    (fun () -> (FromEnvironment (string c, position)) :: xs)
                    c xs

            /// Tags the input symbol as originating from the environment segment and adds it to xs.
            static member addStringFromEnvironment position s xs =
                specialSymbolCata
                    (fun () -> (FromEnvironment (s, position)) :: xs)
                    '!' xs

            /// Concatenates all inputs in s to xs.
            static member concatAllToOutput inputs xs =
                List.map EnvironmentString.get inputs @ xs

            /// Concatenates all environment segment inputs in s to xs.
            static member concatEnvironmentToOutput s xs =
                let environmentInputs =
                    s
                    |> List.choose (function
                        | FromEnvironment (s', _) ->
                            Some s'
                        | _ ->
                            None)
                environmentInputs @ xs

            /// Concatenates all position-tagged environment segment inputs to xs and sorts by position.
            static member concatEnvironmentToProduction s xs =
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
    }


    /// <summary>
    /// Applies a compiled rule to a word.
    /// </summary>
    /// <param name="verbose">If true, displays the state of the state machine at each step.</param>
    /// <param name="rule">The rule to apply.</param>
    /// <param name="word">The word to transform.</param>
    let transform verbose rule word =
        // Debug output columns
        //  is valid transition
        //  position in word    
        //  last output position
        //  transition
        //  --------------------
        //  output buffer
        //  production buffer
        //  undo buffer

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
        }
        |> onError (fun position input current value _ ->
            let nextOutput =
                match value.isPartialMatch, value.wasLastFinal with
                // The rule failed to match
                | true, false ->
                    EnvironmentString.concatAllToOutput value.undo value.output
                // The rule failed to match completely, but what did match was enough to commit the production (i.e. any remaining nodes were optional).
                | true, true ->
                    // undo and production will never be populated at the same time
                    (List.map fst value.production) @ EnvironmentString.concatAllToOutput value.undo value.output
                // The rule has not yet begun to match.
                | false, _ ->
                    EnvironmentString.addOutput input value.output
            if verbose then
                let lastOutputPosition =
                    value.lastOutputOn
                    |> Option.map string
                    |> Option.defaultValue ""
                printf $"   %2d{position} %c{input} %2s{lastOutputPosition}: "
                printfn $"Error at %-11O{current} | %A{nextOutput} [] []"
            Restart {
                value with
                    isPartialMatch = false
                    lastOutputOn = Some position
                    production = []
                    undo = []
                    output = nextOutput
            })
        |> onTransition (fun position transition _ symbol current nextState value ->
            let { lastOutputOn = lastOutputOn; production = production; undo = undo; output = output } = value
            if verbose then
                let lastOutputPosition =
                    lastOutputOn
                    |> Option.map string
                    |> Option.defaultValue ""
                printf $" • %2d{position} %c{symbol} %2s{lastOutputPosition}: "
            let isNextFinal = State.isFinal nextState
            let tf = Map.tryFind transition transformations
            let nextUndo, nextProduction, nextOutput =
                match isNextFinal, nextState, tf with
                // Completed match in environment segment with no transformation.
                // Commit the symbol and combine the undo and production buffers.
                | true, IsEnvironment _, None ->
                    let nextProduction =
                        production
                        |> EnvironmentString.addProduction symbol position
                        |> EnvironmentString.concatEnvironmentToProduction undo
                    [], nextProduction, output

                // Completed match with a transformation.
                // Set production to the output of the transformation and add undo to output.
                | true, IsEnvironment _, Some tf' ->
                    let nextProduction = [tf' + string symbol, position]
                    let nextOutput = EnvironmentString.concatEnvironmentToOutput undo output
                    [], nextProduction, nextOutput

                | true, IsInput _, Some tf' ->
                    let nextProduction = [tf', position]
                    let nextOutput = EnvironmentString.concatEnvironmentToOutput undo output
                    [], nextProduction, nextOutput

                | false, IsEnvironment _, Some tf' ->
                    let nextUndo = EnvironmentString.addFromEnvironment position symbol undo
                    let nextProduction = (tf', position) :: production[1..]
                    nextUndo, nextProduction, output

                // Partial match in environment segment.
                // Add symbol to undo.
                | false, IsEnvironment _, _ ->
                    let nextUndo = EnvironmentString.addFromEnvironment position symbol undo
                    nextUndo, production, output

                // Partial match in input segment with a transformation.
                // Set production to the output of the transformation and add symbol to undo.
                | false, IsInput _, Some tf' ->
                    let nextProduction = [tf', position]
                    let nextUndo = EnvironmentString.addFromInput position symbol undo
                    nextUndo, nextProduction, output

                // Partial match in input segment with no transformation.
                // Add symbol to undo.
                | false, IsInput _, None ->
                    let nextUndo = EnvironmentString.addFromInput position symbol undo
                    nextUndo, production, output

                | _ ->
                    undo, production, output

            if verbose then
                let transition = $"{current} -> {nextState}"
                printfn $"%-20s{transition} | %A{nextOutput} %A{nextProduction} %A{nextUndo}"
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
