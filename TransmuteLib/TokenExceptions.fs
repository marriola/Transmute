namespace TransmuteLib.Exceptions

open TransmuteLib
open System

type UnexpectedTokenException(expected:TokenType, got:Token) =
    inherit SyntaxException(sprintf "Expected '%s', got '%s'" (expected.ToString()) (got.tokenType.ToString()), got.position)

type ExpectedSetException(expected:TokenType list, got:Token) =
    inherit ApplicationException(
        (sprintf "Expected one of %s, got '%s' at row %d, column %d"
            (String.concat ", " (List.map string expected))
            (got.tokenType.ToString()) <|| got.position))
