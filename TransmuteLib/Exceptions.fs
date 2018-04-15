module Exceptions

open System

type SyntaxException(message, pos) =
    inherit ApplicationException(
        let row, col = pos
        in sprintf "%s at row %d, column %d" message row col)
    override this.Message = message
    member this.Position = pos
