namespace TransmuteLib

[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module Exceptions =
    open Exceptions

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
        invalidSyntax got.position message
