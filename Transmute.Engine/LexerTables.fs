// Project:     Transmute.Engine
// Module:      Lexer transition tables
// Copyright:   (c) 2026 Matt Arriola
// License:     MIT

namespace Transmute.Engine

open StateMachine

type InputFormat =
    | IPA
    | X_SAMPA
with
    override this.ToString() =
        match this with
        | IPA -> "IPA"
        | X_SAMPA -> "X-SAMPA"

module private LexerTables =

    /// <summary>
    /// Removes the initial semicolon from the comment and trims whitespace.
    /// </summary>
    /// <param name="token">The comment token.</param>
    let private trimComment token = { token with value = token.value.[1..].Trim() }

    /// Removes the initial sigil from a token if it has one.
    let private trimSigil sigil token =
        { token with
            value =
                if token.value.Length > 0 && token.value.[0] = sigil then
                    token.value.[1..]
                else
                    token.value }

    let START = State.make "START"
    let ERROR = State.make "ERROR"

    let internal Q_Whitespace = State.make "Q_Whitespace"
    let internal Q_WhitespaceFinal = State.make "Q_WhitespaceFinal" |> State.makeFinal
    let internal Q_LBrack = State.make "Q_LBrack" |> State.makeFinal
    let internal Q_RBrack = State.make "Q_RBrack" |> State.makeFinal
    let internal Q_LBrace = State.make "Q_LBrace" |> State.makeFinal
    let internal Q_RBrace = State.make "Q_RBrace" |> State.makeFinal
    let internal Q_LParen = State.make "Q_LParen" |> State.makeFinal
    let internal Q_RParen = State.make "Q_RParen" |> State.makeFinal
    let internal Q_Separator = State.make "Q_Separator" |> State.makeFinal
    let internal Q_Comma = State.make "Q_Comma" |> State.makeFinal
    let internal Q_Divider = State.make "Q_Divider" |> State.makeFinal
    let internal Q_Arrow = State.make "Q_Arrow"
    let internal Q_ArrowFinal = State.make "Q_ArrowFinal" |> State.makeFinal
    let internal Q_Empty = State.make "Q_Empty" |> State.makeFinal
    let internal Q_Placeholder = State.make "Q_Placeholder" |> State.makeFinal
    let internal Q_WordBoundary = State.make "Q_WordBoundary" |> State.makeFinal
    let internal Q_Dollar = State.make "Q_Dollar"
    let internal Q_SyllableBoundary = State.make "Q_SyllableBoundaryFinal" |> State.makeFinal
    let internal Q_Plus = State.make "Q_Plus" |> State.makeFinal
    let internal Q_Minus = State.make "Q_Minus" |> State.makeFinal
    let internal Q_Pipe = State.make "Q_Pipe" |> State.makeFinal
    let internal Q_Not = State.make "Q_Not" |> State.makeFinal
    let internal Q_Equals = State.make "Q_EqualsFinal" |> State.makeFinal
    let internal Q_Identifier = State.make "Q_Identifier"
    let internal Q_IdentifierFinal = State.make "Q_IdentifierFinal" |> State.makeFinal
    let internal Q_Utterance = State.make "Q_Utterance"
    let internal Q_UtteranceFinal = State.make "Q_UtteranceFinal" |> State.makeFinal
    let internal Q_Comment = State.make "Q_Comment"
    let internal Q_CommentFinal = State.make "Q_CommentFinal" |> State.makeFinal

    // Defines the transition table for the lexer.
    let private createTable inputFormat =
        let beginIdentifierTransitions =
            onMany [ seq { 'A'..'Z' } ] Q_Identifier

        let identifierTransitions =
            onMany
                [ seq { '0'..'9' }; seq { 'A'..'Z' }; seq { 'a'..'z' } ]
                Q_Identifier

        let utteranceTransitions =
            if inputFormat = IPA then
                onMany
                    [ // IPA symbols
                      seq { 'a'..'z' }
                      seq { '\u0250'..'\u02ff' }
                      "àáâãäèéêëìíîïòóõôöùúûüỳýŷÿ" :> char seq
                      "æœðøçɸβθχŋ" :> char seq
                      // Combining diacritics
                      seq { '\u0300'..'\u0341' }
                    ]
                    Q_Utterance
            else
                onMany
                    [ seq { 'a'..'z' }
                      seq { 'A'..'Z' }
                      seq { '0'..'9' }
                      "àáâãäèéêëìíîïòóõôöùúûüỳýŷÿ" :> char seq
                      @"?&@{}""%:_\<>`'~=" :> char seq
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
                      @".?&@{}""%:" :> char seq
                    ]
                    Q_Utterance

        let whitespaceTransitions = onMany [ " \t\r\n" :> char seq ] Q_Whitespace

        createTransitionTableFromClasses
            [ makeTransitions (From START) whitespaceTransitions
              makeTransitions (From Q_Whitespace) whitespaceTransitions
              makeTransitions (From Q_Whitespace) [ To Q_WhitespaceFinal, OnEpsilon ]

              // Single character symbols

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
                  To Q_ArrowFinal, OnChar '→'
                  To Q_Empty, OnChar '∅'
                  To Q_Empty, OnChar 'Ø'
                  To Q_Placeholder, OnChar '_'
                  To Q_WordBoundary, OnChar '#'
                  To Q_SyllableBoundary, OnChar 'σ'
                  To Q_Plus, OnChar '+'
                  To Q_Pipe, OnChar '|'
                  To Q_Not, OnChar '!'
                  To Q_Equals, OnChar '='
                ]

              // Arrow symbol, ASCII variant (->)

              makeTransitions (From START) [ To Q_Arrow, OnChar '-' ]
              makeTransitions (From Q_Arrow) [ To Q_ArrowFinal, OnChar '>' ]
              makeTransitions (From Q_Arrow) [ To Q_Minus, OnEpsilon ]

              // Syllable boundary symbol, ASCII variant ($)
              // Doubles as a sigil for identifiers in X-SAMPA mode

              makeTransitions (From START) [ To Q_Dollar, OnChar '$' ]
              makeTransitions (From Q_Dollar) [ To Q_SyllableBoundary, OnEpsilon ]

              // Identifier

              if inputFormat = IPA then
                  makeTransitions (From START) beginIdentifierTransitions

              if inputFormat = X_SAMPA then
                  makeTransitions (From Q_Dollar) identifierTransitions
                  makeTransitions (From Q_LBrack) identifierTransitions
                  makeTransitions (From Q_Plus) identifierTransitions
                  makeTransitions (From Q_Minus) identifierTransitions

              makeTransitions (From Q_Identifier) identifierTransitions
              makeTransitions (From Q_Identifier) [ To Q_IdentifierFinal, OnEpsilon ]

              // Utterance

              makeTransitions (From START) beginUtteranceTransitions
              makeTransitions (From Q_Utterance) utteranceTransitions
              makeTransitions (From Q_Utterance) [ To Q_UtteranceFinal, OnEpsilon ]

              // Comment

              makeTransitions (From START) [ To Q_Comment, OnChar ';' ]
              makeTransitions (From Q_Comment) [ To Q_Comment, OnAny ]
              makeTransitions (From Q_Comment) [ To Q_CommentFinal, OnChar '\n' ]
            ]

    // Maps final states to a tuple of the token type to be produced and a function that modifies the token produced.
    let private stateTokenTypes =
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
          Q_ArrowFinal, Arrow.id
          Q_Empty, Empty.id
          Q_Placeholder, Placeholder.id
          Q_WordBoundary, WordBoundary.id
          Q_SyllableBoundary, SyllableBoundary.id
          Q_Plus, Plus.id
          Q_Minus, Minus.id
          Q_Pipe, Pipe.id
          Q_Not, Not.id
          Q_Equals, Equals.id
          Q_IdentifierFinal, Id.apply (trimSigil '$')
          Q_UtteranceFinal, Utterance.apply (trimSigil '.')
          Q_CommentFinal, Comment.apply trimComment
        ]
        |> Map.ofSeq

    let IPA = createTable IPA
    let X_SAMPA = createTable X_SAMPA

    let actionFor state = Map.tryFind state stateTokenTypes
