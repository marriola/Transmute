open Arguments
open Microsoft.FSharp.Collections
open System
open System.Diagnostics
open System.IO
open TransmuteLib
open TransmuteLib.RuleParser

let trim (s: string) = s.Trim()

let compileRules options features sets rules =
    let indexedRules = List.mapi (fun i n -> (i + 1), n) rules
    let selectedRules =
        match options.testRules with
        | Some ruleNumbers ->
            List.filter
                (fun (i, _) -> List.contains i ruleNumbers)
                indexedRules
        | None ->
            indexedRules

    if options.verbosityLevel > Silent then
        fprintf stderr "Compiling"

    let showNfa = options.verbosityLevel = ShowNFA
    let outerStopwatch = Stopwatch()
    outerStopwatch.Start()

    let rules =
        selectedRules
#if DEBUG
        |> List.map
#else
        |> PSeq.map
#endif
            (fun (i, node) ->
                let rule = RuleCompiler.compile showNfa features sets node
                if options.verbosityLevel > Silent then
                    fprintf stderr "."
                i, node, rule)
        |> PSeq.sortBy (fun (i, _ ,_) -> i)
        |> List.ofSeq

    outerStopwatch.Stop()

    if options.verbosityLevel > Silent then
        fprintfn stderr ""

    outerStopwatch.ElapsedMilliseconds, rules

let transform options rules word =
    let sw = new Stopwatch()

    let rec inner word totalTime rules =
        match rules with
        | [] ->
            word, (float totalTime / 10000.0)
        | (i, ruleDesc, rule)::xs ->
            if options.verbosityLevel >= ShowDFA then
                let transitions, transformations = rule
                printfn "\n********************************************************************************"
                printfn "\nRule %d: %O" i ruleDesc
                printfn "\nDFA:\n"

                transitions
                |> Map.toList
                |> List.iteri (fun j ((fromState, m), toState) ->
                    let t = sprintf "(%O, %O)" fromState m
                    printfn "%d.\t%-35s-> %O" (j + 1) t toState)

                printfn "\ntransformations:"
                transformations
                |> Map.toList
                |> List.iteri (fun j ((From origin, input, To dest), result) ->
                    printfn "%d. (%O, %O) -> %O => %s" (j + 1) origin input dest result)

            sw.Restart()
            let result = RuleMachine.transform (options.verbosityLevel > ShowDFA) rule word
            sw.Stop()

            if options.verbosityLevel >= ShowTransformations && word <> result then
                printfn "%7d. %15s -> %s" i word result

            inner result (totalTime + sw.ElapsedTicks) xs

    inner word 0L rules

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

    let totalCompileTime, rules =
        let compiledFile =
            if Path.GetExtension(options.rulesFile) = ".sc"
                then Path.Combine(Path.GetDirectoryName(options.rulesFile), Path.GetFileNameWithoutExtension(options.rulesFile)) + ".scc"
                else options.rulesFile + ".scc"

        if options.recompile
            || not (File.Exists(compiledFile))
            || File.GetLastWriteTime(options.rulesFile) > File.GetLastWriteTime(compiledFile)
            then
                let features, sets, rules =
                    match parseRulesFile options.rulesFile with
                    | Ok (features, sets, rules) ->
                        features, sets, rules
                    | Result.Error msg ->
                        failwith msg

                let totalCompileTime, rules = compileRules options features sets rules
                RuleCompiler.saveCompiledRules compiledFile rules |> ignore

                totalCompileTime, rules
            else
                let sw = Stopwatch()
                sw.Start()
                let rules = RuleCompiler.readCompiledRules compiledFile
                sw.Stop()

                printfn "Reading rules..."
                sw.ElapsedMilliseconds, rules

    if options.verbosityLevel > Normal then
        fprintfn stderr ""

        List.iter
            (fun (i, node, _) -> printfn "%2d. %O" i node)
            rules

        printfn "\nCompiled %d rules in %d ms (average %.1f ms)\n" rules.Length totalCompileTime (float totalCompileTime / float rules.Length)

    let lexicon =
        IO.File.ReadAllText(options.lexiconFile).Trim().Split('\n')
        |> Array.map trim
        |> Array.indexed
        |> Array.choose (fun (i, word) ->
            match options.testWords with
            | None -> Some word
            | Some testWords when List.contains (i + 1) testWords -> Some word
            | _ -> None)

    let mutable totalMilliseconds = 0.0

    for word in lexicon do
        let result, milliseconds = transform options rules word
        totalMilliseconds <- totalMilliseconds + milliseconds

        if options.verbosityLevel = Normal then
            printfn "%s" result
        else
            printfn "[%5.2f ms] %15s -> %s" milliseconds word result

    if options.verbosityLevel > Normal then
        printf "\nTransformed %d words in %.1f ms (average %.1f ms) \n" lexicon.Length totalMilliseconds (totalMilliseconds / float lexicon.Length)

    0 // return an integer exit code
