open System
open TransmuteLib

let validateOptions (options: Arguments.Options) =
    let mutable isValid = true
    if options.lexiconFile = null then
        printfn "Lexicon not specified"
        isValid <- false
    if options.rulesFile = null then
        printfn "Rules file not specified"
        isValid <- false
    isValid


[<EntryPoint>]
let main argv =
    let options = Arguments.parse argv
    if not (validateOptions options) then
        Environment.Exit(0)
    let result = Lexer.lex options.rulesFile in
        match result with
        | FileError msg ->
            printfn "%s" msg
        | SyntaxError (msg, row, col) ->
            printfn "Syntax error at row %d column %d: %s" row col msg
        | OK tokens ->
            printfn "%s"
                (List.fold
                    (fun acc v -> acc + sprintf "%s\n" (v.ToString()))
                    ""
                    tokens)
    Console.ReadKey()
    0 // return an integer exit code
