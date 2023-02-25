namespace TransmuteLib.Exceptions

open TransmuteLib.Position

exception SyntaxError of string * Offset * Line * Column
