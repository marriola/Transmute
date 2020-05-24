open System
open TransmuteLib
open TransmuteLib.Lexer
open TransmuteLib.Node
open TransmuteLib.SoundChangeRule
open System.Collections.Generic
open System.Diagnostics
open Arguments

let validateOptions (options: Options) =
    let mutable isValid = true
    if options.lexiconFile = null then
        printfn "Lexicon not specified"
        isValid <- false
    if options.rulesFile = null then
        printfn "Rules file not specified"
        isValid <- false
    isValid

let trim (s: string) = s.Trim()

let compileRules options tokens =
    let nodes =
        match RuleParser.parse tokens with
        | Ok nodes -> nodes
        | Result.Error message -> failwith message
    let features = getFeatures nodes
    let sets = getSets nodes

    match SyntaxAnalyzer.validate nodes with
    | Ok () ->
        ()
    | Result.Error message ->
        failwith message

    let sw = new Stopwatch()

    let indexedNodes =
        nodes
        |> List.choose (fun x ->
            match untag x with
            | RuleNode _ as x -> Some x
            | _ -> None)
        |> List.indexed
        |> List.map (fun (i, n) -> (i + 1), n)

    let selectedNodes =
        match options.testRules with
        | Some ruleNumbers ->
            List.filter
                (fun (i, _) -> List.contains i ruleNumbers)
                indexedNodes
        | None ->
            indexedNodes

    fprintf stderr "."

    let result =
        selectedNodes
        |> List.map (fun (_, x) ->
            sw.Restart()
            let rule = SoundChangeRule.compile features sets x
            sw.Stop()
            fprintf stderr "."
            x, rule, sw.ElapsedMilliseconds)

    printfn ""
    result

let transform options rules word =
    let sw = new Stopwatch()

    let rec inner word totalTime rules =
        match rules with
        | [] ->
            word, totalTime
        | (i, (ruleDesc, rule, _))::xs ->
            if options.verbose then
                let transitions, transformations = rule
                printfn "\nRule %d: %O" i ruleDesc
                printfn "\nDFA:\n"

                transitions
                |> Seq.map (fun (pair: KeyValuePair<State * InputSymbol, State>) -> pair.Key, pair.Value)
                |> Seq.indexed
                |> Seq.map (fun (i, ((fromState, m), toState)) ->
                    let t = sprintf "(%O, %O)" fromState m
                    sprintf "%d.\t%-35s-> %O" (i + 1) t toState)
                |> String.concat "\n"
                |> printfn "%s"

                printfn "\ntransformations:"
                transformations
                |> Seq.iteri (fun i (pair: KeyValuePair<Transition<State>, string>) ->
                    let (From origin, input, To dest) = pair.Key
                    let result = pair.Value
                    printfn "%d. (%O, %O) -> %O => %s" (i + 1) origin input dest result)

            sw.Restart()
            if options.verbose then
                printfn "%50s" word

            let result = SoundChangeRule.transform options.verbose rule word
            sw.Stop()
            if options.showTransformations && word <> result then
                printfn "%7d. %15s -> %s" (i + 1) word result
            inner result (totalTime + sw.ElapsedMilliseconds) xs

    rules
    |> List.indexed
    |> List.map (fun (i, r) -> (i + 1), r)
    |> inner word 0L

[<EntryPoint>]
let main argv =
    Console.OutputEncoding <- Text.Encoding.UTF8

#if DEBUG
    // Force .NET to load System.Core so we can inspect enumerables in the debugger
    System.Linq.Enumerable.Count([]) |> ignore
#endif

    let options = Arguments.parse argv

    if not (validateOptions options) then
        Environment.Exit(0)

    match lex options.rulesFile with
    | FileError msg ->
        printfn "%s" msg
    | SyntaxError (msg, row, col) ->
        printfn "Syntax error at row %d column %d: %s" row col msg
    | OK tokens ->
        let rules = compileRules options tokens

        if options.verbose || options.showTransformations then
            fprintfn stderr ""

            rules
            |> List.indexed
            |> List.map (fun (i, (r, _, compileTime)) -> sprintf "%2d. [%4d ms] %O" (i + 1) compileTime r)
            |> String.concat "\n"
            |> printfn "%s"

            let totalCompileTime = List.sumBy (fun (_, _, compileTime) -> compileTime) rules
            printfn "\nTotal compile time: %d ms" totalCompileTime

        let lexicon =
            IO.File.ReadAllText(options.lexiconFile).Trim().Split('\n')
            |> Array.map trim
            |> Array.indexed
            |> Array.choose (fun (i, word) ->
                match options.testWords with
                | None -> Some word
                | Some x when List.contains (i + 1) x -> Some word
                | _ -> None)

        for word in lexicon do
            let result, totalTime = transform options rules word
            printfn "[%3d ms] %15s -> %s" totalTime word result

    0 // return an integer exit code
