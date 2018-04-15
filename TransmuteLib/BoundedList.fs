module BoundedList

type BoundedList<'a> =
    | Begin
    | Item of 'a
    | End

let makeBoundedList xs =
    List.concat
        [ [ Begin ]
          (List.map (fun x -> Item x) xs)
          [ End ]
        ]
