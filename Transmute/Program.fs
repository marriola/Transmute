open Arguments
open System
open System.IO
open TransmuteLib
open TransmuteLib.RuleParser

let formatTime ms =
    if ms > 1000.0 then
        $"%.2f{ms / 1000.0} s"
    else
        $"%.1f{ms} ms"

let trimWhitespace (s: string) = s.Trim()

let time fn =
    let start = DateTime.Now
    let result = fn()
    let stop = DateTime.Now
    let milliseconds = (stop - start).TotalMilliseconds
    result, milliseconds

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

    let rules, elapsed =
        time (fun () ->
            selectedRules
#if DEBUG
            |> List.mapi
#else
            |> Array.ofList
            |> Array.Parallel.mapi
#endif
                (fun _ (i, node) ->
                    let rule, elapsed = time (fun () -> RuleCompiler.compile showNfa features sets node)
                    if options.verbosityLevel > Silent then
                        fprintf stderr "."
                    i, node, rule, elapsed)
            |> List.ofSeq)

    if options.verbosityLevel > Silent then
        fprintfn stderr ""

    rules, elapsed

let transform options rules word =
    let rec inner nextWord log totalTime rules =
        match rules with
        | [] ->
            word, nextWord, log, totalTime
        | (i, ruleDesc, rule, _)::xs ->
            let result, elapsed = time (fun () -> RuleMachine.transform (options.verbosityLevel >= ShowNFA) rule nextWord)

            let log =
                if options.verbosityLevel >= ShowDFA then
                    let transitions, transformations = rule
                    log
                    @ [ $"\nRule {i}: {ruleDesc}\n"
                        "\nDFA:\n\n" ]
                    @ (transitions
                        |> Map.toList
                        |> List.mapi (fun j ((fromState, m), toState) ->
                            let t = $"({fromState}, {m})"
                            $"{j+1}.\t%-35s{t}-> {toState}"))
                    @ [ "\ntransformations:\n" ]
                    @ (transformations
                        |> Map.toList
                        |> List.mapi (fun j ((From origin, input, To dest), result) ->
                            $"{j+1}. ({origin}, {input}) -> {dest} => {result}"))
                    @ [ "\n********************************************************************************" ]
                else
                    log

            let log =
                if options.verbosityLevel >= ShowTransformations && nextWord <> result then
                    log @ [ $"%7d{i}. %15s{nextWord} -> {result}" ]
                else
                    log

            inner result log (totalTime + elapsed) xs

    inner word [] 0.0 rules

let transformLexicon options rules lexicon =
    let result, elapsed =
        time (fun () ->
            lexicon
#if DEBUG
            |> Seq.mapi (fun i word -> transform options rules word)
            |> Seq.toList)
#else
            |> Array.Parallel.mapi (fun i word -> transform options rules word)
            |> Array.toList)
#endif
    result, elapsed

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

    let rules, compileTime =
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

                let rules, compileTime = compileRules options features sets rules
                RuleCompiler.saveCompiledRules compiledFile rules |> ignore

                rules, compileTime
            else
                printfn "Reading rules..."
                time (fun () -> RuleCompiler.readCompiledRules compiledFile)

    if options.verbosityLevel > Normal then
        fprintfn stderr ""

        List.iter
            (fun (i, node, _, milliseconds) ->
                printfn $"[%8s{formatTime milliseconds}] %2d{i}. {node}")
            rules

    // Trim comments, filter to non-empty lines, and select lines if specified on command line

    let trimComment (line: string) =
        let commentIndex = line.IndexOf ";"
        if commentIndex = -1 then line else line[..commentIndex - 1]

    let lexicon =
        IO.File.ReadAllText(options.lexiconFile).Trim().Split('\n')
        |> Array.map (trimComment >> trimWhitespace)
        |> Array.filter (fun ln -> ln.Length > 0 && not (ln.StartsWith ";"))
        |> Array.indexed
        |> Array.choose (fun (i, word) ->
            match options.testWords with
            | None -> Some word
            | Some testWords when List.contains (i + 1) testWords -> Some word
            | _ -> None)

    // Transform lexicon and report

    let transformedLexicon, totalMilliseconds = transformLexicon options rules lexicon

    for original, result, log, milliseconds in transformedLexicon do
        if options.verbosityLevel = Normal then
            printfn "%s" result
        else
            printfn "\n%s" (String.concat "\n" log)
            printfn $"[%5.2f{milliseconds} ms] %15s{original} -> {result}"

    if options.verbosityLevel > Normal then
        let totalTransformMilliseconds =
            transformedLexicon
            |> List.sumBy (fun (_, _, _, milliseconds) -> int milliseconds)
            |> float

        let totalCompileMilliseconds =
            rules
            |> List.sumBy (fun (_, _, _, milliseconds) -> milliseconds)

        let numRules = float rules.Length
        let numWords = float lexicon.Length

        printfn $"\nCompiled {rules.Length} rules in {formatTime compileTime} (average {formatTime (compileTime / numRules)})"
        printfn $"Total compile time {formatTime totalCompileMilliseconds} (average {formatTime (totalCompileMilliseconds / numRules)})"
        printfn $"Transformed {lexicon.Length} words in {formatTime totalMilliseconds} (average {formatTime (totalMilliseconds / numWords)})"
        printfn $"Total transform time {formatTime totalTransformMilliseconds} (average {formatTime (totalTransformMilliseconds / numWords)})"

    0 // return an integer exit code
