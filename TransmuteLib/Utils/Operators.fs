module TransmuteLib.Utils.Operators

let tuple2 x y = (x, y)

let tuple3 x y z = (x, y, z)

let tuple4 w x y z = (w, x, y, z)

module Result =
#if !FABLE_COMPILER
    /// Returns the result if it is Ok, otherwise prints the error and aborts with exit code 1.
    let orAbort = function
        | Ok x ->
            x
        | Error msg ->
            printfn "%s" msg
            exit 1
#endif

    ()

module String =
    let toLower (s: string) = s.ToLower()
    let trim (s: string) = s.Trim()
    let startsWith (substr: string) (s: string) = s.StartsWith substr
