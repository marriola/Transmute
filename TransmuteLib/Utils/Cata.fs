namespace TransmuteLib.Utils

module Cata =
    let optional fSome fNone = function
        | Some i -> fSome i
        | None -> fNone()

    let bool fTrue fFalse = function
        | true -> fTrue()
        | false -> fFalse()
