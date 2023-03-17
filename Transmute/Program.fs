open Arguments
open System
open System.IO
open TransmuteLib

let RULES_EXTENSION = ".sc"
let COMPILED_RULES_EXTENSION = ".scc"

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
        options.testRules
        |> Option.map (fun ruleNumbers -> indexedRules |> List.filter (fun (i, _) -> List.contains i ruleNumbers))
        |> Option.defaultValue indexedRules

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
    let showNfa = options.verbosityLevel >= ShowNFA

    let rec inner nextWord log totalTime rules =
        match rules with
        | [] ->
            word, nextWord, log, totalTime
        | (i, _, rule, _)::xs ->
            let result, elapsed = time (fun () -> RuleMachine.transform showNfa rule nextWord)

            let log =
                if options.verbosityLevel >= ShowTransformations && nextWord <> result then
                    log @ [ $"%7d{i}. %25s{nextWord} -> {result}" ]
                else
                    log

            inner result log (totalTime + elapsed) xs

    inner word [] 0.0 rules

let transformLexicon options rules lexicon =
    let inline transformSerial () = lexicon |> Array.mapi (fun i word -> transform options rules word)
    let inline transformParallel () = lexicon |> Array.Parallel.mapi (fun i word -> transform options rules word)

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

    let rules, compileTime =
        let compiledFile =
            if Path.GetExtension options.rulesFile = RULES_EXTENSION
                then Path.Combine(Path.GetDirectoryName options.rulesFile, Path.GetFileNameWithoutExtension options.rulesFile) + COMPILED_RULES_EXTENSION
                else options.rulesFile + COMPILED_RULES_EXTENSION

        if options.recompile
            || options.rulesFile = "-"
            || not (File.Exists compiledFile)
            || File.GetLastWriteTime options.rulesFile > File.GetLastWriteTime compiledFile
            then
                let result =
                    if options.rulesFile = "-" then
                        let text = Console.In.ReadToEnd().Replace("\r", "\n")
                        RuleParser.parseRules options.format text
                    else
                        RuleParser.parseRulesFile options.format options.rulesFile

                let features, sets, rules =
                    match result with
                    | Ok (features, sets, rules) ->
                        features, sets, rules
                    | Result.Error msg ->
                        fprintfn stderr "%s" msg
                        exit 1

                let rules, compileTime = compileRules options features sets rules

                if options.rulesFile <> "-" then
                    RuleCompiler.saveCompiledRules compiledFile rules |> ignore

                rules, compileTime
            else
                time (fun () -> RuleCompiler.readCompiledRules compiledFile)

    if options.verbosityLevel >= ShowTimings then
        fprintfn stderr ""

        rules
        |> List.filter (fun (i, _, _, _) ->
            match options.testRules with
            | Some testRules -> List.contains i testRules
            | None -> true)
        |> List.iter (fun (i, node, rule, milliseconds) ->
            printfn $"[%8s{formatTime milliseconds}] %2d{i}. {node}"

            if options.verbosityLevel >= ShowDFA then
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

                printfn "\n********************************************************************************")

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

    let transformedLexicon, totalMilliseconds = transformLexicon options rules (Array.ofList lexicon)

    for original, result, log, milliseconds in transformedLexicon do
        if options.verbosityLevel <= Normal then
            printfn "%s" result
        else
            if options.verbosityLevel >= ShowTimings then
                printf $"[%5.2f{milliseconds} ms] %23s{original} -> {result}\n"
            else
                printfn $"%34s{original} -> {result}"

            for line in log do
                printfn "%s" line

            printfn ""

    if options.verbosityLevel >= ShowTimings then
        let totalTransformMilliseconds =
            transformedLexicon
            |> Array.sumBy (fun (_, _, _, milliseconds) -> int milliseconds)
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
        printfn ""

    0 // return an integer exit code
