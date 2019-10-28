namespace TransmuteLib

open System.IO
open TransmuteLib.StateMachine

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
        | Q_Placeholder
        | Q_Boundary
        | Q_Plus
        | Q_Minus
        | Q_Pipe
        | Q_Not
        | Q0
        | Q_Gives
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
            [ transitionFrom START whitespaceTransitions
              transitionFrom Q_Whitespace whitespaceTransitions
              transitionFrom Q_Whitespace [ epsilonTo Q_WhitespaceFinal ]

              transitionFrom START
                [ on '.' Q_Separator
                  on '[' Q_LBrack
                  on ']' Q_RBrack
                  on '{' Q_LBrace
                  on '}' Q_RBrace
                  on '(' Q_LParen
                  on ')' Q_RParen
                  on '/' Q_Divider
                  on '_' Q_Placeholder
                  on '#' Q_Boundary
                  on '+' Q_Plus
                  on '-' Q_Minus
                  on '|' Q_Pipe
                  on '!' Q_Not
              ]

              transitionFrom START [ on '=' Q0 ]
              transitionFrom Q0 [ on '>' Q_Gives ]

              transitionFrom START [ on '$' Q_Identifier ]
              transitionFrom Q_Identifier identifierTransitions
              transitionFrom Q_Identifier [ epsilonTo Q_IdentifierFinal ]

              transitionFrom START utteranceTransitions
              transitionFrom Q_Utterance utteranceTransitions
              transitionFrom Q_Utterance [ epsilonTo Q_UtteranceFinal ]
              transitionFrom START [ on ';' Q_Comment ]
              transitionFrom Q_Comment [ anyTo Q_Comment ]
              transitionFrom Q_Comment [ on '\n' Q_CommentFinal ]
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
        [ Q_WhitespaceFinal, Whitespace.Identity()
          Q_Separator, Separator.Identity()
          Q_LBrack, LBrack.Identity()
          Q_RBrack, RBrack.Identity()
          Q_LBrace, LBrace.Identity()
          Q_RBrace, RBrace.Identity()
          Q_LParen, LParen.Identity()
          Q_RParen, RParen.Identity()
          Q_Divider, Divider.Identity()
          Q_Placeholder, Placeholder.Identity()
          Q_Boundary, Boundary.Identity()
          Q_Plus, Plus.Identity()
          Q_Minus, Minus.Identity()
          Q_Pipe, Pipe.Identity()
          Q_Not, Not.Identity()
          Q_Gives, Gives.Identity()
          Q_IdentifierFinal, Id.Identity()
          Q_UtteranceFinal, Utterance.Identity()
          Q_CommentFinal, Comment.Then(trimComment)
        ]
        |> dict

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
            System.String.Concat(builder |> List.rev |> Array.ofList)

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
            |> onError (fun inputSymbol _ ({ pos = row, col } as value) _ ->
                match value.mismatchAction with
                | MismatchAction.Restart ->
                    ErrorAction.Restart value //{ value with mismatchAction = MismatchAction.Stop }
                | MismatchAction.Stop ->
                    (sprintf "Unrecognized token '%s%c'" ((accumulate value.builder).Trim()) inputSymbol, row, col)
                    |> SyntaxError
                    |> ErrorAction.Stop)
            |> onTransition (fun isEpsilonTransition inputSymbol currentState nextState value ->
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
                    if isNextFinal then
                        let (tokenType, modifyToken) = stateTokenTypes.[nextState]
                        let nextValue =
                            let v = accumulate builder
                            if currentState = Q_Whitespace
                                then v
                                else v.Trim()
                        modifyToken { tokenType = tokenType; position = value.startPos; value = nextValue } :: value.acc
                    else
                        value.acc
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
