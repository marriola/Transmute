// Project:     Transmute
// Module:      RuleCombinators
// Description: Parser combinator primitives
// Copyright:   (c) 2026 Matt Arriola
// License:     MIT

namespace TransmuteLib

module private RuleCombinators =

    type State =
        { status: Status
          rest: string
          matching: bool
          matched: string
          output: string
          undoBuffer: string }

    and Status = Ok | Mismatched

    module State =
        let undo state =
            if state.undoBuffer.Length = 0 then
                state
            else
                { state with
                    status = Ok
                    output = state.undoBuffer
                    undoBuffer = "" }

        let id state = state
            
        let ok state =
            { state with status = Ok }
            
        let fail state =
            { state with status = Mismatched }
        
        let clearUndo state =
            { state with undoBuffer = "" }
            
        let inline isOk state =
            state.status = Ok
    
        let inline map fn state =
            if state.status = Ok then fn state else state
            
        let inline isError fn state =
            if state.status = Mismatched then fn state else state
        
        let inline orElse value state =
            if state.status = Mismatched then value else state

        let wrap input =
            { status = Ok
              rest = input
              output = ""
              matching = false
              matched = ""
              undoBuffer = "" }
        
        let unwrap state =
            state.output

        let unwrapRest state =
            state.rest
        
        let fromOutput state =
            { state with
                rest = state.output
                output = "" }
            
        let outputMatch state =
            if state.matching then
                { state with
                    output = state.output + state.matched
                    matching = false
                    matched = "" }
            elif state.rest.Length > 0 then
                { state with
                    output = state.output + string state.rest.[0]
                    rest = state.rest.Substring 1 }
            else
                state
            
        let clearMatch state =
            if state.matching then
                { state with
                    matching = false
                    matched = "" }
            elif state.rest.Length > 0 then
                { state with
                    output = state.output + string state.rest.[0]
                    rest = state.rest.Substring 1 }
            else
                state

    [<AutoOpen>]
    module internal SoundChangeRule =
        let inline beginRule state =
            { state with undoBuffer = state.output }

        let uncapture = State.outputMatch

        let inline matchSymbols (symbols: string) state =
            state
            |> State.map (fun state ->
                if state.rest.StartsWith symbols then
                    { state with
                        rest = state.rest.Substring symbols.Length
                        matching = true
                        matched = symbols
                        undoBuffer = state.undoBuffer + symbols }
                else
                    State.fail state)

        let inline matchSymbol symbol state =
            state
            |> State.map (fun state ->
                if state.rest.Length > 0 && state.rest.[0] = symbol then
                    let current = string state.rest.[0]
                    { state with
                        rest = state.rest.Substring 1
                        matching = true
                        matched = state.matched + current
                        undoBuffer = state.undoBuffer + current }
                else
                    State.fail state)
                
        let inline matchOptional rule state =
            state
            |> State.map (rule >> State.orElse state)
                
        let inline matchOneOf rules state =
            state
            |> State.map (fun state ->
                (None, rules)
                ||> List.fold (fun result rule ->
                    result
                    |> Option.orElseWith (fun () ->
                        match rule state with
                        | { status = Ok } as result -> Some result
                        | { status = Mismatched } -> None))
                |> Option.defaultWith (fun () ->
                    State.fail state))

        let inline matchWordBoundary state =
            state
            |> State.map (fun state ->
                let symbol =
                    if state.output.Length = 0 && not state.matching then
                        Special.WORD_START_BOUNDARY
                    else
                        Special.WORD_END_BOUNDARY
                matchSymbol symbol state)

        let matchSyllableBoundary =
            function
            | SyllableStart -> Special.SYLLABLE_START_BOUNDARY
            | OnsetStart -> Special.ONSET_START_BOUNDARY
            | OnsetEnd -> Special.ONSET_END_BOUNDARY
            | NucleusStart -> Special.NUCLEUS_START_BOUNDARY
            | NucleusEnd -> Special.NUCLEUS_END_BOUNDARY
            | CodaStart -> Special.CODA_START_BOUNDARY
            | CodaEnd -> Special.CODA_END_BOUNDARY
            | SyllableEnd -> Special.SYLLABLE_END_BOUNDARY
            >> matchSymbol
            
        let inline thenEcho state = State.map State.outputMatch state
                
        let inline thenReplaceWith symbol state =
            state
            |> State.map (fun state ->
                { state with
                    matched = ""
                    matching = false
                    output = state.output + symbol })

        /// <summary>
        /// Replaces the match with enough copies of <c>symbol</c> to match its length.
        /// </summary>
        let inline thenReplaceWithCopies symbol state =
            state
            |> State.map (fun state ->
                let symbol = String.replicate state.matched.Length symbol
                { state with
                    matched = ""
                    matching = false
                    output = state.output + symbol })

        let inline thenDelete state = thenReplaceWith "" state
        
        let inline thenInsert symbol state =
            state
            |> thenEcho
            |> thenReplaceWith symbol
        
        let inline transform input output state =
            state
            |> matchSymbols input
            |> thenReplaceWith output
        
        let inline transformOneOf transformations state =
            let transformRules =
                transformations
                |> List.unzip
                ||> List.map2 transform
            matchOneOf transformRules state
    
        let repeat rule state =
            let rec repeat' state =
                if state.rest.Length > 0 then
                    repeat' (rule state)
                else
                    state
                    |> State.outputMatch
                    |> State.fromOutput

            repeat' state
        
        let endRule = 
            State.map State.clearUndo
            >> State.isError (State.undo >> State.clearMatch)
            >> State.ok

        let inline log i indentLevel message state =
    #if VERBOSE
            let indent = new System.String('\t', indentLevel)
            let stateStr =
                state.ToString().Split '\n'
                |> Seq.map (fun line -> indent + line)
                |> String.concat "\n"

            System.Diagnostics.Debug.WriteLine $"{indent}{i}. {message}"
            System.Diagnostics.Debug.WriteLine stateStr
            System.Diagnostics.Debug.WriteLine (new System.String('-', 40))
    #endif
            state
