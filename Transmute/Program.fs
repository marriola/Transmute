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
            let rules = RuleParser.parse tokens
            printfn "%s"
                (List.fold
                    (fun acc r ->
                        acc + (sprintf
                            "%s\n" 
                            (r.ToString())))
                    ""
                    rules)
            match SyntaxAnalyzer.validate rules with
            | ValidateResult.OK ->
                printfn "Passed validation!"
            | ValidateResult.SyntaxError (message, (row, col)) ->
                printfn "Syntax error at row %d column %d: %s" row col message
    Console.ReadKey()
    0 // return an integer exit code
