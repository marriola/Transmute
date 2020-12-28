namespace TransmuteLib

open System.IO
open TransmuteLib.StateMachine
open TransmuteLib.Utils

module Lexer =
    type Result =
        | OK of Token list
        | SyntaxError of string * int * int
        | FileError of string

    type private State =
        | START
        | ERROR
        | Q_Whitespace
        | Q_WhitespaceFinal
        | Q_Separator
        | Q_LBrack
        | Q_RBrack
        | Q_LBrace
        | Q_RBrace
        | Q_LParen
        | Q_RParen
        | Q_Divider
        | Q0
        | Q_Arrow
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
        let identifierTransitions =
            onMany
                [ seq { '0'..'9' }; seq { 'A'..'Z' }; seq { 'a'..'z' } ]
                Q_Identifier

        let utteranceTransitions =
            onMany
                [ seq { 'a'..'z' }
                  seq { '\u0250'..'\u0341' }
                  "æðøçɸβθχ" :> char seq
                ]
                Q_Utterance

        let whitespaceTransitions = onMany [ " \t\r\n" :> char seq ] Q_Whitespace

        createTransitionTableFromClasses
            [ makeTransitions (From START) whitespaceTransitions
              makeTransitions (From Q_Whitespace) whitespaceTransitions
              makeTransitions (From Q_Whitespace) [ To Q_WhitespaceFinal, OnEpsilon ]

              makeTransitions (From START)
                [ To Q_Separator, OnChar '.'
                  To Q_LBrack, OnChar '['
                  To Q_RBrack, OnChar ']'
                  To Q_LBrace, OnChar '{'
                  To Q_RBrace, OnChar '}'
                  To Q_LParen, OnChar '('
                  To Q_RParen, OnChar ')'
                  To Q_Divider, OnChar '/'
                  To Q_Placeholder, OnChar '_'
                  To Q_Boundary, OnChar '#'
                  To Q_Plus, OnChar '+'
                  To Q_Pipe, OnChar '|'
                  To Q_Not, OnChar '!'
              ]

              makeTransitions (From START) [ To Q0, OnChar '-' ]
              makeTransitions (From Q0) [ To Q_Arrow, OnChar '>' ]
              makeTransitions (From Q0) [ To Q_Minus, OnEpsilon ]

              makeTransitions (From START) [ To Q_Identifier, OnChar '$' ]
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
          Q_Separator, Separator.id
          Q_LBrack, LBrack.id
          Q_RBrack, RBrack.id
          Q_LBrace, LBrace.id
          Q_RBrace, RBrace.id
          Q_LParen, LParen.id
          Q_RParen, RParen.id
          Q_Divider, Divider.id
          Q_Arrow, Arrow.id
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
        { startPos: int * int
          pos: int * int
          mismatchAction: MismatchAction
          builder: char list
          acc: Token list }

    let lex (filename: string) =
        let isFinal state = stateTokenTypes.ContainsKey state
        let accumulate (builder: char list) =
            System.String.Concat(builder |> List.rev |> Array.ofList).Trim()

        try
            use stream = new StreamReader(filename, true)
            let content = stream.ReadToEnd()
            stateMachineConfig()
            |> withTransitions table
            |> withStartState START
            |> withErrorState ERROR
            |> withInitialValue
                { startPos = 1, 1
                  pos = 1, 1
                  mismatchAction = Restart
                  builder = []
                  acc = [] }
            |> onError (fun _ inputSymbol _ ({ pos = row, col } as value) _ ->
                match value.mismatchAction with
                | MismatchAction.Restart ->
                    ErrorAction.Restart value //{ value with mismatchAction = MismatchAction.Stop }
                | MismatchAction.Stop ->
                    (sprintf "Unrecognized token '%s%c'" (accumulate value.builder) inputSymbol, row, col)
                    |> SyntaxError
                    |> ErrorAction.Stop)
            |> onTransition (fun _ _ isEpsilonTransition inputSymbol currentState nextState value ->
                let inline incrRow (row, _) = (row + 1, 1)
                let inline incrCol (row, col) = (row, col + 1)
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
                        let v = accumulate builder
                        let nextValue =
                            Cata.bool
                                (fun _ -> v)
                                (fun _ -> v.Trim())
                                (currentState = Q_Whitespace)
                        (fn value.startPos nextValue) :: value.acc
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
            |> onFinish (fun { acc = acc } ->  acc |> List.rev |> OK)
            |> runStateMachine content
        with
            | ex -> FileError ex.Message
