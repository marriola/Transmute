namespace TransmuteLib

open TransmuteLib.StateMachine

module RuleMachine =
    /// Returns the original value if input is the special begin or end symbol.
    /// Otherwise, returns the result of fTransform.
    let private addChar constructor symbol xs  =
        if symbol = Special.START || symbol = Special.END then
            xs
        else
            constructor (string symbol) :: xs

    /// An input symbol tagged with the action taken when processing it
    type internal BufferString =
        | Unchanged of symbol: string
        | Replaced of symbol: string * original: string
        | Inserted of symbol: string
        | Deleted of count: int * symbol: string
        with
            /// Gets the symbol.
            static member getText = function
                | Unchanged s
                | Replaced (s, _)
                | Inserted s
                | Deleted (_, s) -> s

            static member getOriginal = function
                | Unchanged s
                | Inserted s
                | Deleted (_, s) ->
                    s
                | Replaced (_, original) ->
                    original

            static member withText (text: string) = function
                | Unchanged s -> Unchanged (text)
                | Replaced (s, r) -> Replaced (s, text)
                | Inserted s -> Inserted (text)
                | Deleted (_, s) -> Deleted (text.Length, text)

            static member addProduction value symbol production =
                match production with
                | ReplacesWith (count, s) ->
                    let bufferLength = List.length value.production
                    let skip = min bufferLength count
                    BufferString.coalesce (skip + 1) (Replaced (s, string symbol) :: value.production)
                | Inserts s ->
                    Inserted s :: Unchanged (string symbol) :: value.production
                | Deletes (count, s) ->
                    Deleted (count, string symbol) :: value.production

            /// Returns a list of raw strings with replacements and insertions reversed.
            static member undo xs =
                xs
                |> List.choose (function
                    | Unchanged s
                    | Replaced (_, s)
                    | Deleted (_, s) ->
                        Some s
                    | _ ->
                        None)

            /// Returns a list of raw strings with replacements and insertions.
            static member apply xs =
                let lastInsertIndex = xs |> List.tryFindIndex (function Inserted _ -> true | _ -> false) |> Option.defaultValue -1

                xs
                |> List.indexed
                |> List.choose (fun (i, x) ->
                    match x with
                    | Inserted s when i = lastInsertIndex ->
                        Some s
                    | Unchanged s
                    | Replaced (s, _) ->
                        Some s
                    | _ ->
                        None)

            static member coalesce n xs =
                let n = min n (List.length xs)
                if n < 2 then
                    xs
                else
                    let ys = List.take n xs
                    let ysText = ys |> List.map BufferString.getOriginal |> List.rev |> String.concat ""
                    let head = xs |> List.tryHead |> Option.map (BufferString.withText ysText)
                    head
                    |> Option.map (fun head -> head :: List.skip n xs)
                    |> Option.defaultValue xs

    and internal RuleMachineState = {
        /// Indicates whether the rule has begun to match to the input.
        isPartialMatch: bool

        /// Indicates whether the last state visited was a final state.
        wasLastFinal: bool

        /// The last input position visited.
        lastOutputOn: int option

        /// The current production.
        production: BufferString list

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

        let transitions, transformations = rule

        stateMachineConfig()
        |> withTransitions transitions
        |> withStartState RuleCompiler.START
        |> withErrorState RuleCompiler.ERROR
        |> withInitialValue {
            isPartialMatch = false
            wasLastFinal = false
            lastOutputOn = None
            output = []
            production = []
        }
        |> onError (fun position input current value _ ->
            let nextOutput =
                match value.isPartialMatch, value.wasLastFinal with
                // The rule failed to match
                | true, false ->
                    BufferString.undo value.production @ value.output
                // The rule failed to match completely, but what did match was enough to commit the production (i.e. any remaining nodes were optional).
                | true, true ->
                    BufferString.apply value.production @ value.output
                // The rule has not yet begun to match.
                | false, _ ->
                    if input = Special.START || input = Special.END then
                        value.output
                    else
                        string input :: value.output
            if verbose then
                let lastOutputPosition =
                    value.lastOutputOn
                    |> Option.map string
                    |> Option.defaultValue ""
                printf $"   %2d{position} %c{input} %2s{lastOutputPosition}: "
                printfn $"Error at %-15O{current} | %A{nextOutput} [] []"
            Restart {
                value with
                    isPartialMatch = false
                    lastOutputOn = Some position
                    production = []
                    output = nextOutput
            })
        |> onTransition (fun position transition _ symbol current nextState value ->
            let { lastOutputOn = lastOutputOn; production = production; output = output } = value
            if verbose then
                let lastOutputPosition =
                    lastOutputOn
                    |> Option.map string
                    |> Option.defaultValue ""
                printf $" • %2d{position} %c{symbol} %2s{lastOutputPosition}: "
            let isNextFinal = State.isFinal nextState
            let tf = Map.tryFind transition transformations

            let nextProduction =
                match tf with
                | None ->
                    addChar Unchanged symbol production
                | Some tf ->
                    BufferString.addProduction value symbol tf

            if verbose then
                let transition = $"{current} -> {nextState}"
                printfn $"%-24s{transition} | %A{output} %A{nextProduction}"
            { value with
                isPartialMatch = true
                wasLastFinal = isNextFinal
                lastOutputOn = Some position
                production = nextProduction
            })
        |> onFinish (fun value ->
            // Flush last production
            let nextOutput =
                match value.wasLastFinal, value.production with
                | false, _ -> value.output
                //| true, [] -> BufferString.undo value.production @ value.output
                //| true, _ -> (List.map BufferString.getText value.production) @ value.output
                | true, _ -> BufferString.apply value.production @ value.output
            nextOutput
            |> List.rev
            |> String.concat "")
        |> runStateMachine (string Special.START + word + string Special.END)
