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
#if DEBUG
    // Force .NET to load System.Core so we can inspect enumerables in the debugger
    System.Linq.Enumerable.Count([]) |> ignore
#endif
    Console.OutputEncoding <- Text.Encoding.UTF8
    let options = Arguments.parse argv
    if not (validateOptions options) then
        Environment.Exit(0)
    match Lexer.lex options.rulesFile with
    | FileError msg ->
        printfn "%s" msg
    | SyntaxError (msg, row, col) ->
        printfn "Syntax error at row %d column %d: %s" row col msg
    | OK tokens ->
        let rules = RuleParser.parse tokens
        let features = getFeatures rules
        let sets = getSets rules

        match SyntaxAnalyzer.validate rules with
        | ValidateResult.OK ->
            printfn "Passed validation!"
        | ValidateResult.SyntaxError (message, (row, col)) ->
            printfn "Syntax error at row %d column %d: %s" row col message

        let transitions = SoundChangeRule.createStateMachine features sets rules.[8]

        transitions
            |> List.indexed
            |> List.map (fun (i, ((fromState, m), toState)) -> sprintf "%d.\t(%s, %s)\t-> %s" i (string fromState) (string m) (string toState))
            |> String.concat "\n"
            |> Console.WriteLine

        List.item 4 transitions |> fst |> fst
        |> SoundChangeRule.computeFollowSet transitions
        |> List.map (fun (input, ``to``) -> sprintf "%s, %s" (string input) (string ``to``))
        |> String.concat "\n"
        |> Console.WriteLine

    (Console.ReadKey())
    0 // return an integer exit code
