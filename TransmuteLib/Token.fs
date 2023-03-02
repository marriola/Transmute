namespace TransmuteLib

open TransmuteLib.Position

type TokenType =
    /// An error token.
    | Error

    /// A whitespace token.
    | Whitespace

    /// A '.' token.
    | Separator

    /// A '[' token.
    | LBrack

    /// A ']' token.
    | RBrack

    /// A '{' token.
    | LBrace

    /// A '}' token.
    | RBrace

    /// A '(' token.
    | LParen

    /// A ')' token.
    | RParen

    /// A ',' token.
    | Comma

    /// A '/' token.
    | Divider

    /// A '->' token.
    | Arrow

    /// A '∅' token.
    | Empty

    /// A '_' token.
    | Placeholder

    /// A '#' token.
    | Boundary

    /// A '+' token.
    | Plus

    /// A '-' token.
    | Minus

    /// A '|' token.
    | Pipe

    /// A '!' token.
    | Not

    /// An identifier token.
    | Id

    /// An utterance token.
    | Utterance

    /// A comment token.
    | Comment

type Token =
    { tokenType: TokenType
      position: Offset * Line * Column
      value: string
    }

type Token with
    static member make tokenType position value =
        { tokenType = tokenType;
          position = position;
          value = value
        }

type TokenType with
    /// <summary>
    /// Creates the matched token and returns it unmodified.
    /// </summary>
    member this.id position value =
        { tokenType = this; position = position; value = value }


    /// <summary>
    /// Creates the matched token and applies a function to it.
    /// </summary>
    /// <param name="fn"></param>
    member this.apply (fn: (Token -> Token)) position value =
        fn { tokenType = this; position = position; value = value }

module Token =
    /// Matches a token on its type.
    let (|OfType|_|) tokenType token = 
        match token with
        | { tokenType = tt } as x when tt = tokenType ->
            Some x
        | _ ->
            None

    let (|NewlineWhitespace|_|) ({ tokenType = tokenType; value = value } as token) =
        if tokenType = Whitespace && value.Contains "\n" then
            Some token
        else
            None

    let (|NonNewlineWhitespace|_|) ({ tokenType = tokenType; value = value } as token) =
        if tokenType = Whitespace && not (value.Contains "\n") then
            Some token
        else
            None
