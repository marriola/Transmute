// Project:     Transmute.Engine
// Module:      Position
// Description: Text position type
// Copyright:   (c) 2023 Matt Arriola
// License:     MIT

module Transmute.Engine.Position

type Offset = Offset of int
and Line = Line of int
and Column = Column of int

let inline getOffset (Offset offset, _, _) = offset
let inline getLine (_, Line line, _) = line
let inline getColumn (_, _, Column column) = column
