// Project:     Transmute
// Module:      Program
// Description: Console driver for TransmuteLib
// Copyright:   (c) 2023 Matt Arriola
// License:     MIT

open Arguments
open System
open System.IO
open TransmuteLib
open Utils

let RULES_EXTENSION = ".sc"
let COMPILED_RULES_EXTENSION = ".scc"

let compileRules options features sets syllableDefinition rules =
    if options.verbosityLevel > Silent then
        fprintf stderr "Compiling"

    let showNfa = options.verbosityLevel = ShowNFA

    // TODO re-enable showing the NFA here
    //let syllableRule = syllableDefinition |> Option.map (RuleCompiler.compileSyllableRule showNfa features sets)
    let syllableRule = syllableDefinition |> Option.map (RuleCompiler.compileSyllableRule false features sets)

    let rules, rulesTime =
        time (fun () ->
            let r = new Random()
            rules
            |> List.sortBy (fun _ -> r.Next()) // Shuffle the workload to keep heavy rules (maybe) evenly distributed
#if DEBUG
            |> List.mapi
#else
            |> Array.ofList
            |> Array.Parallel.mapi
#endif
                (fun _ (i, node) ->
                    // TODO re-enable showing the NFA here
                    //let rule, elapsed = time (fun () -> RuleCompiler.compileRule showNfa features sets node)
                    let rule, elapsed = time (fun () -> RuleCompiler.compileRule false features sets node)
                    if options.verbosityLevel > Silent then
                        fprintf stderr "."
                    i, (node, rule, elapsed))
            |> List.ofSeq
            |> List.sortBy fst)

    if options.verbosityLevel > Silent then
        fprintfn stderr ""

    syllableRule, rules, rulesTime

let transformWord options syllableRule rules word =
    let showNfa = options.verbosityLevel >= ShowNFA
    let syllableBoundaryLocations, segmentLocations =
        syllableRule
        |> Option.map (fun rule -> SyllableBoundaryDetector.get showNfa rule word)
        |> Option.defaultValue ([], Map.empty)

    let rec inner (syllableBoundaryLocations: int list, segmentLocations: Map<int, SyllableSegment>) nextWord log totalTime rules =
        match rules with
        | [] ->
            word, nextWord, log, totalTime

        | (i, (_, rule, _))::xs ->
            let (result, locations), elapsed = time (fun () -> Transducer.transformWithChangeLocations showNfa syllableBoundaryLocations rule nextWord)

            let (syllableBoundaryLocations, segmentLocations), syllableBoundaryTime =
                if syllableRule <> None && result <> nextWord then
                    time (fun () ->
                        syllableRule
                        |> Option.map (fun rule -> SyllableBoundaryDetector.get showNfa rule result)
                        |> Option.defaultValue ([], Map.empty))
                else
                    (syllableBoundaryLocations, segmentLocations), 0.0

            let log =
                if options.verbosityLevel >= ShowTransformations && nextWord <> result then
                    let segmentLine =
                        seq { 0..result.Length }
                        |> Seq.map (fun i ->
                            segmentLocations
                            |> Map.tryFind i
                            |> function
                                | Some Onset -> 'O'
                                | Some Nucleus -> 'N'
                                | Some Coda -> 'C'
                                | None -> ' ')
                    let syllableLine =
                        if syllableBoundaryLocations = [] then
                            []
                        else
                            let line = 
                                seq { 0..result.Length }
                                |> Seq.map (fun i -> if List.contains i syllableBoundaryLocations then 'S' else ' ')
                            [ "    " + String.Join("", segmentLine) ] @ [ "    " + String.Join("", line) ]

                    if options.format = IPA then
                        log @ syllableLine @ Transducer.getIpaChangeLine i locations result
                    else
                        log @ syllableLine @ Transducer.getXsampaChangeLine i locations result
                else
                    log

            inner (syllableBoundaryLocations, segmentLocations) result log (totalTime + elapsed + syllableBoundaryTime) xs

    inner (syllableBoundaryLocations, segmentLocations) word [] 0.0 rules

let transformLexicon options syllableRule rules lexicon =
    let inline transformSerial () = lexicon |> Array.map (fun word -> transformWord options syllableRule rules word)
    let inline transformParallel () = lexicon |> Array.Parallel.map (fun word -> transformWord options syllableRule rules word)

#if DEBUG
    let fTransform = transformSerial
#else
    let fTransform = if options.verbosityLevel = ShowNFA && Array.length lexicon > 1 then transformSerial else transformParallel
#endif

    time fTransform

[<EntryPoint>]
let main argv =
    Console.OutputEncoding <- Text.Encoding.UTF8

#if DEBUG
    // Force .NET to load System.Core so we can inspect enumerables in the debugger
    System.Linq.Enumerable.Count([]) |> ignore
#endif

    let options = Arguments.parse argv

    if not (Arguments.validate options) then
        Environment.Exit(0)

    // Compile rules if the compiled rules file is stale or not present, otherwise load the compiled rules file

    let precompiled, ((syllableRule, rules), compileTime) =
        let compiledFile =
            if Path.GetExtension options.rulesFile = RULES_EXTENSION
                then Path.Combine(Path.GetDirectoryName options.rulesFile, Path.GetFileNameWithoutExtension options.rulesFile) + COMPILED_RULES_EXTENSION
                else options.rulesFile + COMPILED_RULES_EXTENSION

        if options.recompile
            || options.testRules <> None
            || options.rulesFile = "-"
            || not (File.Exists compiledFile)
            || File.GetLastWriteTime options.rulesFile > File.GetLastWriteTime compiledFile then
                let parseResult =
                    if options.rulesFile = "-" then
                        let text = Console.In.ReadToEnd().Replace("\r", "\n")
                        RuleParser.parseRules options.format text
                    else
                        RuleParser.parseRulesFile options.format options.rulesFile

                let syllableDefinition, features, sets, rules =
                    parseResult
                    |> Result.mapError (fprintfn stderr "%s")
                    |> Result.toOption
                    |> Option.get

                let indexedRules = List.mapi (fun i n -> (i + 1), n) rules

                let selectedRules =
                    options.testRules
                    |> Option.map (List.map (fun i -> indexedRules[i - 1]))
                    |> Option.defaultValue indexedRules

                let syllableRule, rules, compileTime = compileRules options features sets syllableDefinition selectedRules

                if options.saveRules && options.testRules = None && options.rulesFile <> "-" then
                    RuleCompiler.saveCompiledRules compiledFile (syllableRule, rules) |> ignore

                false, ((syllableRule, rules), compileTime)
            else
                true, time (fun () -> RuleCompiler.readCompiledRules compiledFile)

    // List selected rules

    if options.listRules || options.verbosityLevel >= ShowTransformations then
        fprintfn stderr ""

        for i, (node, _, milliseconds) in rules do
            if options.verbosityLevel >= ShowTimes then
                printf $"[%8s{formatTime milliseconds}] "

            printfn $"%2d{i}. {node}"

        printfn ""

    if options.listRules then
        exit 0

    // Dump rule DFAs

    if false && options.verbosityLevel >= ShowDFA then
        for i, (node, rule, _) in rules do
            let transitions, transformations = rule
            printfn $"\nRule {i}: {node}"

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

        printfn ""

    // Trim comments, filter to non-empty lines, and select lines if specified on command line

    let trimComment (line: string) =
        let commentIndex = line.IndexOf ";"
        if commentIndex = -1 then line else line[..commentIndex - 1]

    let lexicon =
        options.lexiconFiles
        |> List.collect (fun file ->
            if file = "-" then
                Console.In.ReadToEnd().Trim().Replace("\r", "\n").Split('\n') |> List.ofArray
            else
                (new StreamReader(file)).ReadToEnd().Trim().Split('\n') |> List.ofArray)
        |> List.map (trimComment >> trimWhitespace)
        |> List.filter (fun ln -> ln.Length > 0 && not (ln.StartsWith ";"))
        |> List.indexed
        |> List.choose (fun (i, word) ->
            match options.testWords with
            | None -> Some word
            | Some testWords when List.contains (i + 1) testWords -> Some word
            | _ -> None)

    // Transform lexicon and report

    let transformedLexicon, totalMilliseconds = transformLexicon options syllableRule rules (Array.ofList lexicon)
    let outputStream =
        match options.outputFile with
        | None -> Console.Out
        | Some path -> new StreamWriter(path)

    for original, result, log, milliseconds in transformedLexicon do
        if options.verbosityLevel <= Normal then
            fprintfn outputStream "%s" result
        else
            if options.verbosityLevel = ShowTransformations then
                fprintf outputStream "    "
            elif options.verbosityLevel >= ShowTimes then
                fprintf outputStream $"[%5.2f{milliseconds} ms] "
            
            fprintfn outputStream $"{original} -> {result}"

            if options.verbosityLevel >= ShowTransformations then
                fprintfn outputStream ""

                for line in log do
                    if options.verbosityLevel >= ShowTimes then
                        fprintf outputStream "       "
                    fprintfn outputStream "%s" line

            fprintfn outputStream ""

    if options.outputFile <> None then
        outputStream.Flush()
        outputStream.Close()

    // Time report

    if options.verbosityLevel >= ShowTimes then
        let totalTransformMilliseconds =
            transformedLexicon
            |> Array.sumBy (fun (_, _, _, milliseconds) -> int milliseconds)
            |> float

        let totalCompileMilliseconds =
            rules
            |> List.sumBy (fun (_, (_, _, milliseconds)) -> milliseconds)

        let numRules = float rules.Length
        let numWords = float lexicon.Length

        if precompiled then
            printfn $"Loaded {rules.Length} rules in {formatTime compileTime}"
        else
            printfn $"Compiled {rules.Length} rules in {formatTime compileTime} (average {formatTime (compileTime / numRules)})"
        printfn $"Total compile time {formatTime totalCompileMilliseconds} (average {formatTime (totalCompileMilliseconds / numRules)})"
        printfn $"Transformed {lexicon.Length} words in {formatTime totalMilliseconds} (average {formatTime (totalMilliseconds / numWords)})"
        printfn $"Total transform time {formatTime totalTransformMilliseconds} (average {formatTime (totalTransformMilliseconds / numWords)})"
        printfn ""

    0 // return an integer exit code
