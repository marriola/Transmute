namespace TransmuteLib

open System.IO
open TransmuteLib.StateMachine
open TransmuteLib.Token

type LexerState =
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

type LexerResult =
    | OK of (Token list)
    | SyntaxError of (string * int * int)
    | FileError of (string)

module Lexer =
    open System.Text
    open System

    /// <summary>
    /// Defines the transition table for the lexer.
    /// </summary>
    let table =
        let identifierTransitions =
            onMany
                [ seq { '0'..'9' };
                  seq { 'A'..'Z' };
                  seq { 'a'..'z' } ]
                Q_Identifier

        let utteranceTransitions =
            onMany
                [ seq { 'a'..'z' };
                  seq { '\u0250'..'\u0341' };
                  "æðøçɸβθχ" :> char seq ]
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
    let stateTokenTypes =
        (dict
            [ (Q_WhitespaceFinal, Whitespace.Identity())
              (Q_Separator, Separator.Identity())
              (Q_LBrack, LBrack.Identity())
              (Q_RBrack, RBrack.Identity())
              (Q_LBrace, LBrace.Identity())
              (Q_RBrace, RBrace.Identity())
              (Q_LParen, LParen.Identity())
              (Q_RParen, RParen.Identity())
              (Q_Divider, Divider.Identity())
              (Q_Placeholder, Placeholder.Identity())
              (Q_Boundary, Boundary.Identity())
              (Q_Plus, Plus.Identity())
              (Q_Minus, Minus.Identity())
              (Q_Pipe, Pipe.Identity())
              (Q_Not, Not.Identity())
              (Q_Gives, Gives.Identity())
              (Q_IdentifierFinal, Id.Identity())
              (Q_UtteranceFinal, Utterance.Identity())
              (Q_CommentFinal, Comment.Then(trimComment))
          ])

    let incrRow pos =
        let row, _ = pos
        (row + 1, 1)

    let incrCol pos =
        let row, col = pos
        (row, col + 1)

    let isFinal state =
        stateTokenTypes.ContainsKey state

    let lex (filename: string) =
        try
            use stream = new StreamReader(filename, true)
            stream.ReadToEnd()
            |> runStateMachine
                { transitionTable = table;
                  startState = START;
                  errorState = ERROR;
                  initialValue = ((1, 1), (1, 1), new StringBuilder(), []);
                  transitionFromStartOnFail = true }
                (fun next pos (_, (row, col), value, _) ->
                    (sprintf "Unrecognized token '%s'" (value.ToString().Trim()), row, col)
                    |> SyntaxError)
                (fun isNextFinal isEpsilon inputSymbol currentState nextState (startPos, pos, value: StringBuilder, acc) ->
                    let nextPos =
                        if isEpsilon then
                            pos
                        else if inputSymbol = '\n' then
                            incrRow pos
                        else
                            incrCol pos
                    if not isEpsilon then
                        value.Append(inputSymbol) |> ignore
                    // Add token to output if on a final state
                    let nextAcc =
                        if isNextFinal then
                            let (tokenType, modifyToken) = stateTokenTypes.[nextState]
                            let nextValue =
                                if currentState = Q_Whitespace then
                                    value.ToString()
                                else
                                    value.ToString().Trim()
                            modifyToken { tokenType = tokenType; position = startPos; value = nextValue } :: acc
                        else
                            acc
                    let nextStartPos =
                        // Reset startPos when finishing a match, and don't set it until the next non-whitespace character
                        if isNextFinal || (value.Length = 0 && currentState <> Q_Whitespace && System.Char.IsWhiteSpace(inputSymbol)) then
                            nextPos
                        else
                            startPos
                    if isNextFinal then
                        value.Clear() |> ignore
                    nextStartPos, nextPos, value, nextAcc)
                (fun state -> isFinal state)
                (fun (_, _, _, acc) -> List.rev acc |> OK)

        with
            | ex -> FileError ex.Message
