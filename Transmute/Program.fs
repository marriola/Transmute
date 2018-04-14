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
    Console.OutputEncoding <- Text.Encoding.UTF8
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
                    (fun acc v ->
                        acc + (sprintf
                            "type: %s\n\
                                value: \"%s\"\n\
                                position: (%d, %d)\n\n" 
                            (v.tokenType.ToString())
                            v.value
                            <|| v.position))
                    ""
                    tokens)
    Console.ReadKey()
    0 // return an integer exit code
