namespace TransmuteLib

open TransmuteLib.Exceptions
open TransmuteLib.Position

[<AutoOpen>]
module ExceptionHelpers =
    let syntaxErrorMessage message (Offset offset, Line line, Column column) =
        $"Syntax error at line {line}, column {column} (offset {offset}): {message}"

    let invalidSyntax message (offset, line, col) = raise (SyntaxError (message, offset, line, col))

    /// <summary>
    /// Raises a <see cref="SyntaxException"/> for an unexpected token.
    /// </summary>
    /// <param name="expected">The types of tokens that were expected.</param>
    /// <param name="got">The unexpected token that was encountered.</param>
    let unexpectedToken (expected: TokenType list) got =
        let message =
            sprintf "Expected one of %s, got '%s'"
                (expected |> List.map string |> String.concat ",")
                (string got.tokenType)
        invalidSyntax message got.position
