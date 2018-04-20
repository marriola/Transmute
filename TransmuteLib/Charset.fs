module Charset

/// Creates a range of characters.
let make (lower: char) (upper: char) =
    let rec makeInternal i j out =
        if i < j then
            out
        else
            makeInternal (i - 1) j ((char i) :: out)
    in
        makeInternal (int upper) (int lower) []
