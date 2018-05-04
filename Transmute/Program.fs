open System
open TransmuteLib
open TransmuteLib.RuleParser
open TransmuteLib.Node

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
            let features = getFeatures rules
            let sets = getSets rules
            // Get SetIdentifierNode from first rule and create prefix tree
            let rule =
                match untag rules.[1] with
                | RuleNode (target, _, _) ->
                    let tree = PrefixTree.fromSet sets.["$V"] //.fromSetIntersection sets features target.[0]
                    printfn "%s" (string tree)
                rules
                    |> List.fold
                        (fun acc r ->
                            acc + (sprintf "%s\n" <| string r))
                        ""
                    |> printfn "%s"
            match SyntaxAnalyzer.validate rules with
            | ValidateResult.OK ->
                printfn "Passed validation!"
            | ValidateResult.SyntaxError (message, (row, col)) ->
                printfn "Syntax error at row %d column %d: %s" row col message
            let r = SoundChangeRule.createStateMachine features sets rules.[1]
            printfn "%s" (string r)
    Console.ReadKey()
    0 // return an integer exit code
