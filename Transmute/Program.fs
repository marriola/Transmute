open System
open TransmuteLib
open TransmuteLib.Lexer
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
    match lex options.rulesFile with
    | FileError msg ->
        printfn "%s" msg
    | SyntaxError (msg, row, col) ->
        printfn "Syntax error at row %d column %d: %s" row col msg
    | OK tokens ->
        let nodes = RuleParser.parse tokens
        let features = getFeatures nodes
        let sets = getSets nodes

        match SyntaxAnalyzer.validate nodes with
        | ValidateResult.OK ->
            printfn "Passed validation!"
        | ValidateResult.SyntaxError (message, (row, col)) ->
            printfn "Syntax error at row %d column %d: %s" row col message

        let rules =
            nodes
            |> List.choose (fun x ->
                match untag x with
                | RuleNode _ as x -> Some x
                | _ -> None)

        rules
        |> List.indexed
        |> List.map (fun (i, r) -> sprintf "%d. %s" i (string r))
        |> String.concat "\n"
        |> printfn "%s"

        printf "? "
        let selection = Console.ReadLine() |> int
        let rule = SoundChangeRule.compile features sets rules.[selection]

        printf "\nDFA:\n\n"

        rule
        |> Seq.map (fun pair -> pair.Key, pair.Value)
        |> Seq.indexed
        |> Seq.map (fun (i, ((fromState, m), toState)) -> sprintf "%d.\t(%s, %s)\t-> %s" i (string fromState) (string m) (string toState))
        |> String.concat "\n"
        |> Console.WriteLine

        match SoundChangeRule.matchWord rule "snt" with
        | SoundChangeRule.Result.Mismatch -> printf "no match\n"
        | SoundChangeRule.Result.Match result -> printf "match: %s\n" result

    (Console.ReadKey())
    0 // return an integer exit code
