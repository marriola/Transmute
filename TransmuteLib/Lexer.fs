namespace TransmuteLib

open System.IO

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

type LexerState = {
    stream: StreamReader;
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

    let rec lexInternal (state: LexerState) =
        let nextChar = char (state.stream.Peek())
        let nextRow = if nextChar = '\n' then state.row + 1 else state.row
        let nextCol = if nextChar = '\n' then 1 else state.col + 1

        let inline nextValue () = state.lastValue + (state.stream.Read() |> char |> string)

        let inline addToken out t v pos =
            { tokenType = t; value = v; position = pos } :: out

        let inline advance tokenType = {
            stream = state.stream;
            startPosition = (nextRow, nextCol)
            row = nextRow;
            col = nextCol;
            lastValue = "";
            out = (addToken state.out tokenType (nextValue()) state.startPosition)
            }

        let inline continueValue () = {
            stream = state.stream;
            startPosition = state.startPosition;
            row = nextRow;
            col = nextCol;
            lastValue = nextValue();
            out = state.out
            }

        if state.stream.EndOfStream then
            OK (List.rev state.out)
        else
            match nextChar with
            | '[' -> lexInternal (advance LBrack)
            | ']' -> lexInternal (advance RBrack)
            | '{' -> lexInternal (advance LBrace)
            | '}' -> lexInternal (advance RBrace)
            | '(' -> lexInternal (advance LParen)
            | ')' -> lexInternal (advance RParen)
            | '/' -> lexInternal (advance Divider)
            | '_' -> lexInternal (advance Placeholder)
            | '#' -> lexInternal (advance Boundary)
            | '+' -> lexInternal (advance Plus)
            | '-' -> lexInternal (advance Minus)
            | '|' -> lexInternal (advance Pipe)
            | '!' -> lexInternal (advance Not)
            | '=' ->
                
            | ' ' | '\t' | '\r' | '\n' -> lexInternal (continueValue())
            | x -> SyntaxError (sprintf "Unrecognized token '%c'" x, state.row, state.col)

    let lex (filename: string) =
        try
            lexInternal {
                stream = new StreamReader(filename, true);
                startPosition = (1, 1);
                row = 1;
                col = 1;
                lastValue = "";
                out = []
                }
        with
            | ex -> FileError ex.Message
