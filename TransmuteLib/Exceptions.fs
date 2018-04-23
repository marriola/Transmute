namespace TransmuteLib.Exceptions

open System

type SyntaxException(message, pos) =
    inherit ApplicationException(
        let row, col = pos
        in sprintf "%s at line %d, column %d" message row col)
    override this.Message = message
    member this.Position = pos

type UnrecognizedTokenException(value, pos) =
    inherit ApplicationException(sprintf "Unrecognized token '%s' at line %d, column %d" value <|| pos)
