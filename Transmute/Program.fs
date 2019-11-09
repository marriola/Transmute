open System
open TransmuteLib
open TransmuteLib.Lexer
open TransmuteLib.Node
open TransmuteLib.SoundChangeRule
open System.Diagnostics

let validateOptions (options: Arguments.Options) =
    let mutable isValid = true
    if options.lexiconFile = null then
        printfn "Lexicon not specified"
        isValid <- false
    if options.rulesFile = null then
        printfn "Rules file not specified"
        isValid <- false
    isValid

let trim (s: string) = s.Trim()

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
        let nodes =
            match RuleParser.parse tokens with
            | Ok nodes -> nodes
            | Result.Error message -> failwith message
        let features = getFeatures nodes
        let sets = getSets nodes

        match SyntaxAnalyzer.validate nodes with
        | Ok () ->
            printfn "Passed validation!"
        | Result.Error message ->
            failwith message

        let rules =
            nodes
            |> List.choose (fun x ->
                match untag x with
                | RuleNode _ as x -> Some x
                | _ -> None)

        rules
        |> List.indexed
        |> List.map (fun (i, r) -> sprintf "%d. %O" (i + 1) r)
        |> String.concat "\n"
        |> printfn "%s"

        printf "? "
        let selections =
            Console.ReadLine().Split(',')
            |> Array.map (trim >> int >> (+) -1)
            |> Array.toList

        let mutable word = "kʲmtˈom"
        let sw = new Stopwatch()

        // TODO: Figure out how to correctly apply transformations that are followed by non-producing transitions

        for selection in selections do
            let rule = rules.[selection]

            printfn "\nRule %d: %O" (selection + 1) rule

            sw.Restart()

            let rule = SoundChangeRule.compile features sets rule
            let transitions, transformations = rule

            sw.Stop()
            printfn "Compiled in %d ms" sw.ElapsedMilliseconds

            printfn "\nDFA:\n"

            transitions
            |> Seq.map (fun pair -> pair.Key, pair.Value)
            |> Seq.indexed
            |> Seq.map (fun (i, ((fromState, m), toState)) ->
                let t = sprintf "(%s, %s)" (string fromState) (string m)
                sprintf "%d.\t%-35s-> %s" i t (string toState))
            |> String.concat "\n"
            |> Console.WriteLine

            printfn "\ntransformations:"
            transformations
            |> Seq.iteri (fun i kvp ->
                let (From origin, input, To dest) = kvp.Key
                let result = kvp.Value
                printfn "%d. (%O, %O) -> %O => %s" (i + 1) origin input dest result)

            printfn ""

            sw.Restart()

            match SoundChangeRule.transform rule word with
            | Result.Error _ ->
                printf "[%d ms] no match\n" sw.ElapsedMilliseconds
            | Result.Ok result ->
                sw.Stop()
                word <- result
                printf "[%d ms] result: %s\n" sw.ElapsedMilliseconds result

    0 // return an integer exit code
