namespace TransmuteLib

open System.IO
open StateMachine

type TokenType =
    | Error
    | Whitespace
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

type Token = {
    tokenType: TokenType;
    position: int * int;
    value: string
    }

type State =
    | START
    | ERROR
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

type LexerState = {
    stream: StreamReader;
    currentState: State;
    startPosition: int * int;
    row: int;
    col: int;
    lastValue: string;
    out: Token list;
    }

type LexerResult =
    | OK of (Token list)
    | SyntaxError of (string * int * int)
    | FileError of (string)

module Lexer =
    open System.Text

    let identifierTransitions =
        List.concat (
            [
                List.map (transitionTo Q_Identifier) (Charset.make '0' '9')
                List.map (transitionTo Q_Identifier) (Charset.make 'A' 'Z')
                List.map (transitionTo Q_Identifier) (Charset.make 'a' 'z')
                [ on '-' Q_Identifier ]
            ]
        )

    let utteranceTransitions =
        List.concat (
            [
                List.map (transitionTo Q_Utterance) (Charset.make 'a' 'z')
                List.map (transitionTo Q_Utterance) (Charset.make '\u0250' '\u0341')
                List.map (transitionTo Q_Utterance) (List.ofSeq "æðøçθβɸ")
            ]
        )
    
    let table =
        createTransitionTable
            [
                transitionFrom START (List.map (transitionTo START) (List.ofSeq " \t\r\n"))

                transitionFrom START
                    [
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

    let stateTokens =
        (dict
            [
                (Q_LBrack, LBrack)
                (Q_RBrack, RBrack)
                (Q_LBrace, LBrace)
                (Q_RBrace, RBrace)
                (Q_LParen, LParen)
                (Q_RParen, RParen)
                (Q_Divider, Divider)
                (Q_Placeholder, Placeholder)
                (Q_Boundary, Boundary)
                (Q_Plus, Plus)
                (Q_Minus, Minus)
                (Q_Pipe, Pipe)
                (Q_Not, Not)
                (Q_Gives, Gives)
                (Q_IdentifierFinal, Id)
                (Q_UtteranceFinal, Utterance)
                (Q_CommentFinal, Comment)
            ])

    let incrRow pos =
        let row, col = pos
        (row + 1, 1)

    let incrCol pos =
        let row, col = pos
        (row, col + 1)

    let rec lexInternal
        (stream: StreamReader)
        (value: StringBuilder)
        (currentState: State)
        (startPos: int * int)
        (pos: int * int)
        (out: Token list) =

        let rec step s c =
            let matchSymbol, next = StateMachine.step table s c ERROR
            // If we can't step from the current state, try stepping from START
            if next = ERROR && stateTokens.ContainsKey currentState then
                step START c
            else
                (matchSymbol, next)

        if stream.EndOfStream then
            OK (List.rev out)
        else
            let nextChar = char (stream.Peek())
            let matchSymbol, nextState = step currentState nextChar
            let isFinal = stateTokens.ContainsKey nextState
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
                    value.Append(stream.Read() |> char |> string))

            if nextState = ERROR then
                let row, col = startPos in
                    SyntaxError (sprintf "Unrecognized token '%s'" (value.ToString().Trim()), row, col)
            else
                let nextOut =
                    if isFinal then
                        { tokenType = stateTokens.[nextState]; value = value.ToString().Trim(); position = startPos } :: out
                    else
                        out

                lexInternal
                    stream
                    (if isFinal then value.Clear() else value)
                    nextState
                    (if isFinal || System.Char.IsWhiteSpace(nextChar) then nextPos else startPos)
                    nextPos
                    nextOut

    let lex (filename: string) =
        try
            use stream = new StreamReader(filename, true)
            lexInternal stream (new StringBuilder()) START (1, 1) (1, 1) []
        with
            | ex -> FileError ex.Message
