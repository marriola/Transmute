﻿open System
open TransmuteLib
open TransmuteLib.Lexer
open TransmuteLib.Node
open System.Collections.Generic
open System.Diagnostics
open Arguments
open Microsoft.FSharp.Collections

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

    let showNfa = options.verbosityLevel.showNfa()
    let outerStopwatch = Stopwatch()
    outerStopwatch.Start()

    let rules =
        selectedNodes
        |> List.indexed
        |> PSeq.map (fun (i, (_, node)) ->
            let rule = RuleCompiler.compile features sets node showNfa
            fprintf stderr "."
            i, (node, rule))
        |> PSeq.sortBy fst
        |> PSeq.map snd
        |> List.ofSeq

    outerStopwatch.Stop()

    fprintfn stderr ""
    outerStopwatch.ElapsedMilliseconds, rules

let transform options rules word =
    let sw = new Stopwatch()

    let rec inner word totalTime rules =
        match rules with
        | [] ->
            word, totalTime
        | (i, (ruleDesc, rule))::xs ->
            if options.verbosityLevel.verbose() then
                let transitions, transformations = rule
                printfn "\nRule %d: %O" i ruleDesc
                printfn "\nDFA:\n"

                transitions
                |> Seq.map (fun (pair: KeyValuePair<State * InputSymbol, State>) -> pair.Key, pair.Value)
                |> Seq.mapi (fun j ((fromState, m), toState) ->
                    let t = sprintf "(%O, %O)" fromState m
                    sprintf "%d.\t%-35s-> %O" (j + 1) t toState)
                |> String.concat "\n"
                |> printfn "%s"

                printfn "\ntransformations:"
                transformations
                |> Seq.iteri (fun j (pair: KeyValuePair<Transition<State>, string>) ->
                    let (From origin, input, To dest) = pair.Key
                    let result = pair.Value
                    printfn "%d. (%O, %O) -> %O => %s" (j + 1) origin input dest result)

            sw.Restart()
            if options.verbosityLevel.verbose() then
                printfn "%50s" word

            let result = RuleMachine.transform (options.verbosityLevel.verbose()) rule word
            sw.Stop()

            if options.verbosityLevel.showTransformations() && word <> result then
                printfn "%7d. %15s -> %s" i word result

            inner result (totalTime + sw.ElapsedTicks) xs

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
        let totalCompileTime, rules = compileRules options tokens

        if options.verbosityLevel <> Silent then
            fprintfn stderr ""

            rules
            |> List.indexed
            |> List.map (fun (i, (node, _)) -> sprintf "%2d. %O" (i + 1) node)
            |> String.concat "\n"
            |> printfn "%s"

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
            let totalMilliseconds = (float totalTime / 10000.0)
            printfn "[%5.2f ms] %15s -> %s" totalMilliseconds word result

    0 // return an integer exit code
