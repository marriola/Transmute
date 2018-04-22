namespace TransmuteLib

open System.IO
open Exceptions
open StateMachine

type TokenType =
    | Error
    | Whitespace
    | Separator
    | LBrack
    | RBrack
    | LBrace
    | RBrace
    | LParen
    | RParen
    | Divider
    | Placeholder
    | Boundary
    | Gives
    | Plus
    | Minus
    | Pipe
    | Not
    | Id
    | Utterance
    | Comment

type Token =
    { tokenType: TokenType;
      position: int * int;
      value: string
    }

type Token with
    static member identity token = token
    static member make tokenType position value =
        { tokenType = tokenType;
          position = position;
          value = value
        }

type State =
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

type TokenType with
    /// <summary>
    /// Returns the token created with this type unmodified.
    /// </summary>
    member this.Identity () =
        (this, Token.identity)
    /// <summary>
    /// Applies the function to modify the matched token.
    /// </summary>
    /// <param name="fn"></param>
    member this.Then (fn: Token -> Token) =
        (this, fn)

type LexerResult =
    | OK of (Token list)
    | SyntaxError of (string * int * int)
    | FileError of (string)

module Lexer =
    open System.Text
    open System

    let identifierTransitions =
        List.concat
            [ List.map (transitionTo Q_Identifier) (Charset.make '0' '9')
              List.map (transitionTo Q_Identifier) (Charset.make 'A' 'Z')
              List.map (transitionTo Q_Identifier) (Charset.make 'a' 'z')
            ]

    let utteranceTransitions =
        List.concat
            [ List.map (transitionTo Q_Utterance) (Charset.make 'a' 'z')
              List.map (transitionTo Q_Utterance) (Charset.make '\u0250' '\u0341')
              List.map (transitionTo Q_Utterance) (List.ofSeq "æðøçθβɸ")
            ]

    let whitespaceTransitions =
        List.map (transitionTo Q_Whitespace) (List.ofSeq " \t\r\n")
    
    /// <summary>
    /// Defines the transition table for the lexer.
    /// </summary>
    let table =
        createTransitionTable
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
    let trimComment token = Token.make token.tokenType token.position (token.value.[1..].Trim())

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
        let row, col = pos
        (row + 1, 1)

    let incrCol pos =
        let row, col = pos
        (row, col + 1)

    let isFinal state =
        stateTokenTypes.ContainsKey state

    let rec lexInternal
        (stream: StreamReader)
        (value: StringBuilder)
        (currentState: State)
        (startPos: int * int)
        (pos: int * int)
        (out: Token list) =

        let step nextValue state input =
            let rec stepInternal state transitioningFromStartOnFail =
                let matchSymbol, next = StateMachine.step table state input ERROR
                // If we can't step from the current state, try stepping from START
                if next = ERROR then
                    if transitioningFromStartOnFail then
                        if isFinal currentState then
                            stepInternal START false
                        else
                            matchSymbol, next
                    else
                        raise (UnrecognizedTokenException (nextValue, pos))
                else
                    matchSymbol, next
            stepInternal state true

        if stream.EndOfStream then
            OK (List.rev out)
        else
            // Peek the next input symbol and transition on it. If the transition is not an
            // epsilon transition, advance the lexer position and consume the input symbol.
            let nextChar = char (stream.Peek())
            let nextValue = value.ToString() + (string nextChar)
            let matchSymbol, nextState = step nextValue currentState nextChar
            let isNextFinal = isFinal nextState
            let isEpsilon = matchSymbol = Epsilon
            let nextPos =
                if isEpsilon then
                    pos
                else if nextChar = '\n' then
                    incrRow pos
                else
                    incrCol pos

            ignore (
                if isEpsilon then
                    value
                else
                    value.Append(stream.Read() |> char))

            if nextState = ERROR then
                let row, col = startPos in
                    SyntaxError (sprintf "Unrecognized token '%s'" (value.ToString().Trim()), row, col)
            else
                // Add token to output if on a final state
                let nextOut =
                    if isNextFinal then
                        let (tokenType, modifyToken) = stateTokenTypes.[nextState]
                        let nextValue =
                            if currentState = Q_Whitespace then
                                value.ToString()
                            else
                                value.ToString().Trim()
                        modifyToken (Token.make tokenType startPos nextValue) :: out
                    else
                        out
                let nextStartPos =
                    // Reset startPos when finishing a match, and don't set it until the next non-whitespace character
                    if isNextFinal || (value.Length = 0 && currentState <> Q_Whitespace && System.Char.IsWhiteSpace(nextChar)) then
                        nextPos
                    else
                        startPos

                if isNextFinal then
                    ignore (value.Clear())

                lexInternal stream value nextState nextStartPos nextPos nextOut

    let lex (filename: string) =
        try
            use stream = new StreamReader(filename, true)
            lexInternal stream (new StringBuilder()) START (1, 1) (1, 1) []
        with
            | ex -> FileError ex.Message
