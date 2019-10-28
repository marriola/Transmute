namespace TransmuteLib

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

    /// A '/' token.
    | Divider

    /// A '_' token.
    | Placeholder

    /// A '#' token.
    | Boundary

    /// A '=>' token.
    | Gives

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

module Token =
    /// Matches a token on its type.
    let (|OfType|_|) tokenType token = 
        match token with
        | { tokenType = tt } as x when tt = tokenType ->
            Some x
        | _ ->
            None
