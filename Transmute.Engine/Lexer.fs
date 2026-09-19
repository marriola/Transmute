// Project:     Transmute.Engine
// Module:      Lexer
// Copyright:   (c) 2026 Matt Arriola
// License:     MIT

namespace Transmute.Engine

open Transmute.Engine.Position
open Transmute.Engine.StateMachine

module private Lexer =
    type Result =
        | OK of Token list
        | SyntaxError of string * Offset * Line * Column

    type MismatchAction = Restart | Stop

    type LexerValue =
        { startPos: Offset * Line * Column
          pos: Offset * Line * Column
          mismatchAction: MismatchAction
          builder: char list
          acc: Token list }

    let lex inputFormat (content: string) =
        let accumulate (builder: char list) =
            System.String.Concat(builder |> List.rev |> Array.ofList)

        let inline incrRow (Offset offset, Line row, _) = (Offset (offset + 1), Line (row + 1), Column 1)
        let inline incrCol (Offset offset, Line row, Column col) = (Offset (offset + 1), Line row, Column (col + 1))

        let table =
            match inputFormat with
            | IPA -> LexerTables.IPA
            | X_SAMPA -> LexerTables.X_SAMPA

        stateMachineConfig()
        |> withTransitions table
        |> withStartState LexerTables.START
        |> withErrorState LexerTables.ERROR
        |> withInitialValue
            { startPos = Offset 0, Line 1, Column 1
              pos = Offset 0, Line 1, Column 1
              mismatchAction = Restart
              builder = []
              acc = [] }
        |> onError (fun inputSymbol machineState ->
            let { currentValue = value } = machineState
            let { pos = offset, row, col } = value
            match value.mismatchAction with
            | MismatchAction.Restart ->
                ErrorAction.Restart value //{ value with mismatchAction = MismatchAction.Stop }
            | MismatchAction.Stop ->
                (sprintf "Unrecognized token '%s%c'" (accumulate value.builder) inputSymbol, offset, row, col)
                |> SyntaxError
                |> ErrorAction.Stop)
        |> onTransition (fun inputSymbol t machineState ->
            let (_, input, To nextState) = t
            let { currentValue = value; currentState = currentState } = machineState
            let isEpsilonTransition = input = OnEpsilon
            let isNextFinal = State.isFinal nextState

            let nextPos =
                if isEpsilonTransition then
                    value.pos
                else if inputSymbol = '\n' then
                    incrRow value.pos
                else
                    incrCol value.pos
            let builder =
                if not isEpsilonTransition
                    then inputSymbol :: value.builder
                    else value.builder
            // Add token to output if on a final state
            let nextAcc =
                match LexerTables.actionFor nextState with
                | Some fn ->
                    let v = builder |> accumulate |> fn value.startPos
                    v :: value.acc
                | None -> value.acc
            let nextStartPos =
                // Reset startPos when finishing a match, and don't set it until the next non-whitespace character
                if isNextFinal
                    || (builder = []
                        && currentState <> LexerTables.Q_Whitespace
                        && System.Char.IsWhiteSpace(inputSymbol))
                    then nextPos
                    else value.startPos
            { value with
                    startPos = nextStartPos
                    pos = nextPos
                    mismatchAction = if isNextFinal then MismatchAction.Restart else MismatchAction.Stop
                    builder = if isNextFinal then [] else builder
                    acc = nextAcc })
        |> onFinish (fun ({ acc = acc }) ->
            acc |> List.rev |> OK)
        |> runDFA (content + "\n")
