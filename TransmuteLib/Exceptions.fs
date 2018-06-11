namespace TransmuteLib

open System

/// The exception that is thrown when a syntax error has been encountered.
type SyntaxException(message, pos) =
    inherit ApplicationException(
        let row, col = pos
        sprintf "%s at line %d, column %d" message row col)
    override this.Message = message
    member this.Position = pos

module Exceptions =
    let invalidSyntax position message = raise (SyntaxException(message, position))
