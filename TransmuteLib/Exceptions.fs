// Copyright 2023 Matt Arriola

// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:

// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.

namespace TransmuteLib

open TransmuteLib.Exceptions
open TransmuteLib.Position
open System.Text.RegularExpressions

module ExceptionHelpers =
    let syntaxErrorMessage message (Offset offset, Line line, Column column) =
        sprintf "Syntax error at line %d, column %d (offset %d): %s" line column offset message

    let invalidSyntax message (offset, line, col) = raise (SyntaxError (message, offset, line, col))

    let RE_LAST_COMMA = new Regex(", ([^,]+)$")

    /// <summary>
    /// Raises a <see cref="SyntaxException"/> for an unexpected token.
    /// </summary>
    /// <param name="expected">The types of tokens that were expected.</param>
    /// <param name="got">The unexpected token that was encountered.</param>
    let unexpectedToken (expected: TokenType list) got =
        let tokenList = expected |> List.map string |> String.concat ", "

        let message =
            sprintf "Expected %s%s, got %s"
                (if expected.Length > 1 then "one of " else "")
                (RE_LAST_COMMA.Replace(tokenList, " or $1"))
                (string got.tokenType)
        invalidSyntax message got.position
