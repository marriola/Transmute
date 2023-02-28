namespace TransmuteLib.Utils

module Cata =
    let bool fTrue fFalse = function
        | true -> fTrue()
        | false -> fFalse()
