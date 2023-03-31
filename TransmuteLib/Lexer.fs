// Project:     TransmuteLib
// Module:      Lexer
// Copyright:   (c) 2023 Matt Arriola
// License:     MIT

namespace TransmuteLib

open TransmuteLib.Position
open TransmuteLib.StateMachine

type InputFormat =
    | IPA
    | X_SAMPA

module internal Lexer =
    type Result =
        | OK of Token list
        | SyntaxError of string * Offset * Line * Column
        | FileError of string

    let private START = State.make "START"
    let private ERROR = State.make "ERROR"
    let private Q_Whitespace = State.make "Q_Whitespace"
    let private Q_WhitespaceFinal = State.make "Q_WhitespaceFinal" |> State.makeFinal
    let private Q_LBrack = State.make "Q_LBrack" |> State.makeFinal
    let private Q_RBrack = State.make "Q_RBrack" |> State.makeFinal
    let private Q_LBrace = State.make "Q_LBrace" |> State.makeFinal
    let private Q_RBrace = State.make "Q_RBrace" |> State.makeFinal
    let private Q_LParen = State.make "Q_LParen" |> State.makeFinal
    let private Q_RParen = State.make "Q_RParen" |> State.makeFinal
    let private Q_Separator = State.make "Q_Separator" |> State.makeFinal
    let private Q_Comma = State.make "Q_Comma" |> State.makeFinal
    let private Q_Divider = State.make "Q_Divider" |> State.makeFinal
    let private Q0 = State.make "Q0"
    let private Q_Arrow = State.make "Q_Arrow" |> State.makeFinal
    let private Q_Empty = State.make "Q_Empty" |> State.makeFinal
    let private Q_Placeholder = State.make "Q_Placeholder" |> State.makeFinal
    let private Q_WordBoundary = State.make "Q_WordBoundary" |> State.makeFinal
    let private Q_SyllableBoundary = State.make "Q_SyllableBoundary"
    let private Q_SyllableBoundaryFinal = State.make "Q_SyllableBoundaryFinal" |> State.makeFinal
    let private Q_Plus = State.make "Q_Plus" |> State.makeFinal
    let private Q_Minus = State.make "Q_Minus" |> State.makeFinal
    let private Q_Pipe = State.make "Q_Pipe" |> State.makeFinal
    let private Q_Not = State.make "Q_Not" |> State.makeFinal
    let private Q_Equals = State.make "Q_Equals"
    let private Q_EqualsFinal = State.make "Q_EqualsFinal" |> State.makeFinal
    let private Q_Identifier = State.make "Q_Identifier"
    let private Q_IdentifierFinal = State.make "Q_IdentifierFinal" |> State.makeFinal
    let private Q_Utterance = State.make "Q_Utterance"
    let private Q_UtteranceFinal = State.make "Q_UtteranceFinal" |> State.makeFinal
    let private Q_Comment = State.make "Q_Comment"
    let private Q_CommentFinal = State.make "Q_CommentFinal" |> State.makeFinal

    /// <summary>
    /// Removes the initial semicolon from the comment and trims whitespace.
    /// </summary>
    /// <param name="token">The comment token.</param>
    let trimComment token = { token with value = token.value.[1..].Trim() }

    /// Removes the initial sigil from a token if it has one.
    let trimSigil sigil token = { token with value = if token.value.Length > 0 && token.value[0] = sigil then token.value[1..] else token.value }

    type MismatchAction = Restart | Stop

    type LexerValue =
        { startPos: Offset * Line * Column
          pos: Offset * Line * Column
          mismatchAction: MismatchAction
          builder: char list
          acc: Token list }

    let lex inputFormat (content: string) =
        // Defines the transition table for the lexer.
        let table =
            let beginIdentifierTransitions =
                if inputFormat = IPA then
                    onMany [ seq { 'A'..'Z' } ] Q_Identifier
                else
                    [ To Q_Identifier, OnChar '$' ]

            let identifierTransitions =
                onMany
                    [ seq { '0'..'9' }; seq { 'A'..'Z' }; seq { 'a'..'z' } ]
                    Q_Identifier

            let utteranceTransitions =
                if inputFormat = IPA then
                    onMany
                        [ seq { 'a'..'z' }
                          seq { '\u0250'..'\u0341' }
                          "àáâãäèéêëìíîïòóõôöùúûüỳýŷÿ" :> char seq
                          "æœðøçɸβθχ" :> char seq
                        ]
                        Q_Utterance
                else
                    onMany
                        [ seq { 'a'..'z' }
                          seq { 'A'..'Z' }
                          seq { '0'..'9' }
                          "àáâãäèéêëìíîïòóõôöùúûüỳýŷÿ" :> char seq
                          @"?&@{}""%:_\<>`'~" :> char seq
                        ]
                        Q_Utterance

            let beginUtteranceTransitions =
                if inputFormat = IPA then
                    utteranceTransitions
                else
                    onMany
                        [ seq { 'a'..'z' }
                          seq { 'A'..'Z' }
                          seq { '0'..'9' }
                          "àáâãäèéêëìíîïòóõôöùúûüỳýŷÿ" :> char seq
                          @".?&@{}""%" :> char seq
                        ]
                        Q_Utterance

            let whitespaceTransitions = onMany [ " \t\r\n" :> char seq ] Q_Whitespace

            createTransitionTableFromClasses
                [ makeTransitions (From START) whitespaceTransitions
                  makeTransitions (From Q_Whitespace) whitespaceTransitions
                  makeTransitions (From Q_Whitespace) [ To Q_WhitespaceFinal, OnEpsilon ]

                  makeTransitions (From START)
                    [ To Q_Comma, OnChar ','
                      To Q_Separator, OnChar '.'
                      To Q_LBrack, OnChar '['
                      To Q_RBrack, OnChar ']'
                      To Q_LBrace, OnChar '{'
                      To Q_RBrace, OnChar '}'
                      To Q_LParen, OnChar '('
                      To Q_RParen, OnChar ')'
                      To Q_Divider, OnChar '/'
                      To Q_Arrow, OnChar '→'
                      To Q_Empty, OnChar '∅'
                      To Q_Empty, OnChar 'Ø'
                      To Q_Placeholder, OnChar '_'
                      To Q_WordBoundary, OnChar '#'
                      To Q_Plus, OnChar '+'
                      To Q_Pipe, OnChar '|'
                      To Q_Not, OnChar '!'
                  ]

                  makeTransitions (From START) [ To Q0, OnChar '-' ]
                  makeTransitions (From Q0) [ To Q_Arrow, OnChar '>' ]
                  makeTransitions (From Q0) [ To Q_Minus, OnEpsilon ]

                  makeTransitions (From START) [ To Q_Equals, OnChar '=' ]
                  makeTransitions (From Q_Equals) [ To Q_EqualsFinal, OnEpsilon ]

                  makeTransitions (From START) [ To Q_SyllableBoundary, OnChar '$' ]
                  makeTransitions (From Q_SyllableBoundary) identifierTransitions
                  makeTransitions (From Q_SyllableBoundary) [ To Q_SyllableBoundaryFinal, OnEpsilon ]

                  if inputFormat = X_SAMPA then
                      makeTransitions (From Q_LBrack) identifierTransitions
                      makeTransitions (From Q_Plus) identifierTransitions
                      makeTransitions (From Q_Minus) identifierTransitions

                  makeTransitions (From START) beginIdentifierTransitions
                  makeTransitions (From Q_Identifier) identifierTransitions
                  makeTransitions (From Q_Identifier) [ To Q_IdentifierFinal, OnEpsilon ]

                  makeTransitions (From START) beginUtteranceTransitions
                  makeTransitions (From Q_Utterance) utteranceTransitions
                  makeTransitions (From Q_Utterance) [ To Q_UtteranceFinal, OnEpsilon ]
                  makeTransitions (From START) [ To Q_Comment, OnChar ';' ]
                  makeTransitions (From Q_Comment) [ To Q_Comment, OnAny ]
                  makeTransitions (From Q_Comment) [ To Q_CommentFinal, OnChar '\n' ]
                ]

        // Maps final states to a tuple of the token type to be produced and a function that modifies the token produced.
        let stateTokenTypes =
            [ Q_WhitespaceFinal, Whitespace.id
              Q_Separator, Separator.id
              Q_Comma, Comma.id
              Q_LBrack, LBrack.id
              Q_RBrack, RBrack.id
              Q_LBrace, LBrace.id
              Q_RBrace, RBrace.id
              Q_LParen, LParen.id
              Q_RParen, RParen.id
              Q_Divider, Divider.id
              Q_Arrow, Arrow.id
              Q_Empty, Empty.id
              Q_Placeholder, Placeholder.id
              Q_WordBoundary, WordBoundary.id
              Q_SyllableBoundaryFinal, SyllableBoundary.id
              Q_Plus, Plus.id
              Q_Minus, Minus.id
              Q_Pipe, Pipe.id
              Q_Not, Not.id
              Q_EqualsFinal, Equals.id
              Q_IdentifierFinal, Id.apply (trimSigil '$')
              Q_UtteranceFinal, Utterance.apply (trimSigil '.')
              Q_CommentFinal, Comment.apply trimComment
            ]
            |> Map.ofSeq

        let accumulate (builder: char list) =
            System.String.Concat(builder |> List.rev |> Array.ofList)

        let inline incrRow (Offset offset, Line row, _) = (Offset (offset + 1), Line (row + 1), Column 1)
        let inline incrCol (Offset offset, Line row, Column col) = (Offset (offset + 1), Line row, Column (col + 1))

        stateMachineConfig()
        |> withTransitions table
        |> withStartState START
        |> withErrorState ERROR
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
                match Map.tryFind nextState stateTokenTypes with
                | Some fn ->
                    let v = builder |> accumulate |> fn value.startPos
                    v :: value.acc
                | None -> value.acc
            let nextStartPos =
                // Reset startPos when finishing a match, and don't set it until the next non-whitespace character
                if isNextFinal
                    || (builder = []
                        && currentState <> Q_Whitespace
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
