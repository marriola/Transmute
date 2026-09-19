// Project:     Transmute
// Module:      Program
// Description: Console driver for Transmute.Engine
// Copyright:   (c) 2026 Matt Arriola
// License:     MIT

open Arguments
open System
open System.IO
open Transmute.Engine
open Utils
open Transmute.Engine.Utils.Operators

let dumpRules (rules: CompileRuleResult list) =
    for rule in rules do
        let transitions, transformations = rule.compiledRule
        printfn $"\nRule {rule.lineNumber}: {rule.node}"

        transitions
        |> Map.toList
        |> List.iteri (fun j ((fromState, m), toState) ->
            let t = $"({fromState}, {m})"
            printfn $"{j+1}.\t%-35s{t}-> {toState}")

        printfn "\ntransformations:"

        transformations
        |> Map.toList
        |> List.iteri (fun j ((From origin, input, To dest), result) ->
            printfn $"{j+1}. ({origin}, {input}) -> {dest} => {result}")

        printfn "\n********************************************************************************"

[<EntryPoint>]
let main argv =
    Console.OutputEncoding <- Text.Encoding.UTF8

#if DEBUG
    // Force .NET to load System.Core so we can inspect enumerables in the debugger
    System.Linq.Enumerable.Count([]) |> ignore
#endif

    let options = Arguments.parse argv

    if not (Arguments.validate options) then
        Environment.Exit 0

    let rulesFileOptions =
        let baseOptions =
            RulesFileOptions.Default
            |> RulesFileOptions.withSource (options.rulesFile.GetStream())
            |> RulesFileOptions.withInputFormat options.format
            |> RulesFileOptions.withTestRules options.testRules
            |> RulesFileOptions.withSilence (options.verbosityLevel = Silent)
            |> RulesFileOptions.withParallelism options.parallelism

        if options.verbosityLevel > Silent then
            baseOptions
            |> RulesFileOptions.withProgressHook (fun completedCount ruleCount ->
                Console.Error.Write $"[{completedCount}/{ruleCount}] Compiling...\r")
        else
            baseOptions

    let rulesFile =
        rulesFileOptions
        |> RulesFile.load
        |> Async.RunSynchronously
        |> Result.orAbort

    if options.verbosityLevel > Silent then
        Console.Error.WriteLine "\n"

    // List selected rules

    if options.listRules || options.verbosityLevel >= ShowTransformations then
        rulesFile.rules
        |> List.iter (fun rule ->
            if options.verbosityLevel >= ShowTimes then
                printf $"[%8s{formatTime rule.compileTime}] "

            printfn $"%3d{rule.lineNumber}: {rule.node}")

        printfn ""

    if options.listRules then
        exit 0

    // Dump rule DFAs

    if options.verbosityLevel >= ShowDFA then
        dumpRules rulesFile.rules

        printfn ""

    // Trim comments, filter to non-empty lines, and select lines if specified on command line

    let trimComment (line: string) =
        let commentIndex = line.IndexOf ";"
        if commentIndex = -1 then line else line[..commentIndex - 1]

    let lexicon =
        options.lexiconFiles
        |> List.collect (fun file -> file.Read().Split('\n') |> List.ofArray)
        |> List.map (trimComment >> trimWhitespace)
        |> List.filter (fun line -> line.Length > 0)
        |> List.indexed
        |> List.choose (fun (i, word) ->
            match options.testWords with
            | None -> Some word
            | Some testWords when List.contains (i + 1) testWords -> Some word
            | _ -> None)

    // Transform lexicon and report

    let transformedLexicon, totalMilliseconds = RulesFile.transformLexicon rulesFile (Some Ascii) (Array.ofList lexicon)
    let outputStream =
        match options.outputFile with
        | None -> Console.Out
        | Some path -> new StreamWriter(path)

    let errors = transformedLexicon |> Seq.collect (fun result -> result.errors)

    if not (Seq.isEmpty errors) then
        errors
        |> Seq.iter (fun (lineNumber, message) -> Console.WriteLine $"{lineNumber}: {message}")
        printfn ""

    for result in transformedLexicon do
        if options.verbosityLevel <= Normal then
            fprintfn outputStream "%s" result.nextWord
        else
            if options.verbosityLevel = ShowTransformations then
                fprintf outputStream "    "
            elif options.verbosityLevel >= ShowTimes then
                fprintf outputStream $"[%5.2f{result.totalTime} ms] "
            
            fprintfn outputStream $"{result.original} -> {result.nextWord}"

            if options.verbosityLevel >= ShowTransformations then
                fprintfn outputStream ""

                for line in result.changes do
                    if options.verbosityLevel >= ShowTimes then
                        fprintf outputStream "       "

                    if line.ruleNumber <> -1 then
                        Console.WriteLine $"%3d{line.ruleNumber}: {line.change}"
                    else
                        Console.WriteLine $"     {line.change}"

            fprintfn outputStream ""

    if options.outputFile <> None then
        outputStream.Flush()
        outputStream.Close()

    // Time report

    if options.verbosityLevel >= ShowTimes then
        let totalTransformMilliseconds =
            transformedLexicon
            |> Array.sumBy (fun result -> int (result.totalTime))
            |> float

        let totalCompileMilliseconds = List.sumBy (fun rule -> rule.compileTime) rulesFile.rules

        let numRules = float rulesFile.rules.Length
        let numWords = float lexicon.Length

        if rulesFile.cached then
            printfn $"Loaded {rulesFile.rules.Length} rules in {formatTime rulesFile.totalCompileTime}"
        else
            printfn $"Compiled {rulesFile.rules.Length} rules in {formatTime rulesFile.totalCompileTime} (average {formatTime (rulesFile.totalCompileTime / numRules)})"

        printfn $"Total compile time {formatTime totalCompileMilliseconds} (average {formatTime (totalCompileMilliseconds / numRules)})"
        printfn $"Transformed {lexicon.Length} words in {formatTime totalMilliseconds} (average {formatTime (totalMilliseconds / numWords)})"
        printfn $"Total transform time {formatTime totalTransformMilliseconds} (average {formatTime (totalTransformMilliseconds / numWords)})"
        printfn ""

    0 // return an integer exit code
