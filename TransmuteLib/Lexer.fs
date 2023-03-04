namespace TransmuteLib

open TransmuteLib.Position
open TransmuteLib.StateMachine

module internal Lexer =
    type Result =
        | OK of Token list
        | SyntaxError of string * Offset * Line * Column
        | FileError of string

    type private State =
        | START
        | ERROR
        | Q_Whitespace
        | Q_WhitespaceFinal
        | Q_LBrack
        | Q_RBrack
        | Q_LBrace
        | Q_RBrace
        | Q_LParen
        | Q_RParen
        | Q_Comma
        | Q_Divider
        | Q0
        | Q_Arrow
        | Q_Empty
        | Q_Placeholder
        | Q_Boundary
        | Q_Plus
        | Q_Minus
        | Q_Pipe
        | Q_Not
        | Q_Identifier
        | Q_IdentifierFinal
        | Q_Utterance
        | Q_UtteranceFinal
        | Q_Comment
        | Q_CommentFinal

    /// <summary>
    /// Defines the transition table for the lexer.
    /// </summary>
    let private table =
        let beginIdentifierTransitions =
            onMany
                [ seq { 'A'..'Z' } ]
                Q_Identifier

        let identifierTransitions =
            onMany
                [ seq { '0'..'9' }; seq { 'A'..'Z' }; seq { 'a'..'z' }; "." :> char seq ]
                Q_Identifier

        let utteranceTransitions =
            onMany
                [ seq { 'a'..'z' }
                  seq { '\u0250'..'\u0341' }
                  "æœðøçɸβθχ" :> char seq
                ]
                Q_Utterance

        let whitespaceTransitions = onMany [ " \t\r\n" :> char seq ] Q_Whitespace

        createTransitionTableFromClasses
            [ makeTransitions (From START) whitespaceTransitions
              makeTransitions (From Q_Whitespace) whitespaceTransitions
              makeTransitions (From Q_Whitespace) [ To Q_WhitespaceFinal, OnEpsilon ]

              makeTransitions (From START)
                [ To Q_Comma, OnChar ','
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
                  To Q_Boundary, OnChar '#'
                  To Q_Plus, OnChar '+'
                  To Q_Pipe, OnChar '|'
                  To Q_Not, OnChar '!'
              ]

              makeTransitions (From START) [ To Q0, OnChar '-' ]
              makeTransitions (From Q0) [ To Q_Arrow, OnChar '>' ]
              makeTransitions (From Q0) [ To Q_Minus, OnEpsilon ]

              makeTransitions (From START) beginIdentifierTransitions
              makeTransitions (From Q_Identifier) identifierTransitions
              makeTransitions (From Q_Identifier) [ To Q_IdentifierFinal, OnEpsilon ]

              makeTransitions (From START) utteranceTransitions
              makeTransitions (From Q_Utterance) utteranceTransitions
              makeTransitions (From Q_Utterance) [ To Q_UtteranceFinal, OnEpsilon ]
              makeTransitions (From START) [ To Q_Comment, OnChar ';' ]
              makeTransitions (From Q_Comment) [ To Q_Comment, OnAny ]
              makeTransitions (From Q_Comment) [ To Q_CommentFinal, OnChar '\n' ]
            ]

    /// <summary>
    /// Removes the initial semicolon from the comment and trims whitespace.
    /// </summary>
    /// <param name="token">The comment token.</param>
    let trimComment token = { token with value = token.value.[1..].Trim() }

    /// <summary>
    /// Maps final states to a tuple of the token type to be produced and a function that modifies the token produced.
    /// </summary>
    let private stateTokenTypes =
        [ Q_WhitespaceFinal, Whitespace.id
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
          Q_Boundary, Boundary.id
          Q_Plus, Plus.id
          Q_Minus, Minus.id
          Q_Pipe, Pipe.id
          Q_Not, Not.id
          Q_IdentifierFinal, Id.id
          Q_UtteranceFinal, Utterance.id
          Q_CommentFinal, Comment.apply trimComment
        ]
        |> Map.ofSeq

    type MismatchAction = Restart | Stop

    type LexerValue =
        { startPos: Offset * Line * Column
          pos: Offset * Line * Column
          mismatchAction: MismatchAction
          builder: char list
          acc: Token list }

    let lex (content: string) =
        let isFinal state = stateTokenTypes.ContainsKey state
        let accumulate (builder: char list) =
            System.String.Concat(builder |> List.rev |> Array.ofList)

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
        |> onError (fun _ inputSymbol _ ({ pos = offset, row, col } as value) _ ->
            match value.mismatchAction with
            | MismatchAction.Restart ->
                ErrorAction.Restart value //{ value with mismatchAction = MismatchAction.Stop }
            | MismatchAction.Stop ->
                (sprintf "Unrecognized token '%s%c'" (accumulate value.builder) inputSymbol, offset, row, col)
                |> SyntaxError
                |> ErrorAction.Stop)
        |> onTransition (fun _ _ isEpsilonTransition inputSymbol currentState nextState value ->
            let inline incrRow (Offset offset, Line row, _) = (Offset (offset + 1), Line (row + 1), Column 1)
            let inline incrCol (Offset offset, Line row, Column col) = (Offset (offset + 1), Line row, Column (col + 1))
            let isNextFinal = isFinal nextState
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
        |> onFinish (fun ({ acc = acc }) -> acc |> List.rev |> OK)
        |> runStateMachine (content + "\n")
