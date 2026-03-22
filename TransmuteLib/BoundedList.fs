// Project:     TransmuteLib
// Module:      BoundedList
// Copyright:   (c) 2023 Matt Arriola
// License:     MIT

namespace TransmuteLib

type private BoundedList<'a> =
    | Begin
    | Item of 'a
    | End

type private BoundedList<'a> with
    static member fromList (xs: 'a list) =
        List.concat
            [ [ Begin ]
              (List.map (fun x -> Item x) xs)
              [ End ]
            ]
