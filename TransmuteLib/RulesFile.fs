// Project:     Transmute
// Module:      RulesFile
// Description: Functions for loading rules files and using them to transform lexicons.
// Copyright:   (c) 2026 Matt Arriola
// License:     MIT

namespace TransmuteLib

open System
open System.IO
open TransmuteLib.Utils.Operators

type SyllableDefinitionRule = int * (string -> string)

type CompileRuleResult =
    { lineNumber: int
      compileTime: float
      node: Node
      rule: RuleCompiler.CompiledRule }

module RulesFile =
    type Options =
        { debug: bool
          cached: bool
          recompile: bool
          silent: bool
          showNfa: bool
          testRules: int list option
          format: InputFormat
          source: Stream }
    with
        static member Default =
            { debug = false
              cached = false
              recompile = true
              silent = true
              showNfa = false
              testRules = None
              format = IPA
              source = Console.OpenStandardInput() }

        static member withDebug b options = { options with debug = b }
        static member withCache b options = { options with cached = b }
        static member withRecompile b options = { options with recompile = b }
        static member withSilent b options = { options with silent = b }
        static member withNfaDump b options = { options with showNfa = b }
        static member withTestRules ruleNumbers options = { options with testRules = ruleNumbers }
        static member withInputFormat format options = { options with format = format }
        static member withSource stream options = { options with source = stream }
        static member fromText (text: string) options =
            let memoryStream = new MemoryStream()
            memoryStream.Write(System.Text.Encoding.UTF8.GetBytes text)
            memoryStream.Seek(0, SeekOrigin.Begin) |> ignore
            { options with source = memoryStream }

    type RulesFile =
        { format: InputFormat
          rules: RuleCompiler.CompiledRule list
          ruleNodes: Node list
          lineNumbers: int list
          syllableRules: SyllableDefinitionRule list
          totalCompileTime: float
          compileTimes: float list
          recompiled: bool
          debug: bool }

    let inline private time fn =
        let start = DateTime.Now
        let result = fn()
        let stop = DateTime.Now
        let milliseconds = (stop - start).TotalMilliseconds
        result, milliseconds

    let private compileRules options features sets syllableDefinitions rules =
        if not options.silent then
            fprintf stderr "Compiling"

        let syllableRules =
            syllableDefinitions
            |> List.map (fun (i, node) -> i, SyllableRuleCompiler.compile features sets node)

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
                        let rule, elapsed = time (fun () -> RuleCompiler.compile options.showNfa features sets node)
                        let ruleLine = Node.getLine node
                        if not options.silent then
                            fprintf stderr "."
                        { lineNumber = ruleLine
                          compileTime = elapsed
                          node = node
                          rule = rule })
                |> List.ofSeq
                |> List.sortBy (fun result -> result.lineNumber))

        if not options.silent then
            fprintfn stderr ""

        syllableRules, rules, rulesTime

    let load options =
        if options.recompile then
            let text = (new StreamReader(options.source)).ReadToEnd()
            let parseResult = RuleParser.parseRules options.format text
            let syllableDefinitions, features, sets, rules = Result.orAbort parseResult
            let indexedRules = List.mapi (fun i n -> (i + 1), n) rules

            let selectedRules =
                options.testRules
                |> Option.map (List.map (fun i -> indexedRules[i - 1]))
                |> Option.defaultValue indexedRules

            let syllableRules, rules, compileTime = compileRules options features sets syllableDefinitions selectedRules

            //match options.rulesFile with
            //| File (RulesFile _ as rulesFile) when options.saveRules && options.testRules = None ->
            //    RuleCompiler.saveCompiledRules rulesFile.CompiledRuleFilePath (syllableRule, rules) |> ignore

            //| _ ->
            //    ()

            { format = options.format
              recompiled = true
              debug = options.debug
              totalCompileTime = compileTime
              compileTimes = rules |> List.map (fun rule -> rule.compileTime)
              syllableRules = syllableRules
              ruleNodes = rules |> List.map (fun rule -> rule.node)
              lineNumbers = rules |> List.map (fun rule -> rule.lineNumber)
              rules = rules |> List.map (fun rule -> rule.rule) }
        else
            fprintfn stderr "Loading rules..."
            let (syllableRules, rules), loadTime =
                time (fun () -> RuleCompiler.readCompiledRulesFromStream options.source)

            { format = options.format
              recompiled = false
              debug = options.debug
              totalCompileTime = loadTime
              compileTimes = []
              syllableRules = syllableRules
              ruleNodes = List.map fst rules
              lineNumbers = [] // TODO save line numbers in the compiled rules file. But do I really care to re-enable that if AOT is so fast?
              rules = List.map snd rules }

    let private DIVIDER = new System.String('-', 80)

    /// <summary>
    /// Selects the syllable rule that applies to the given line in the rule set, and detects syllable boundaries and segment locations in the given word.
    /// </summary>
    /// <remark>
    /// When multiple syllable definitions occur in a rule set, each applies to all following rules up to the next syllable definition node, which
    /// replaces the last one from that point on.
    /// </remark>
    let private syllabizeWord debug syllableRules (ruleLine: int) word =
        if debug then
            printfn "%s" DIVIDER
            printfn "Syllabizer"
            printfn "%s" DIVIDER

        match List.tryFind (fst >> (>=) ruleLine) syllableRules with
        | None ->
            Result.Ok (Map.empty, String.Empty)
        | Some (_, syllableRule) ->
    #if VERBOSE
            System.Diagnostics.Debug.WriteLine (">>>> " + word)
    #endif
            SyllableBoundaryDetector.findBoundaries syllableRule word

    type TransformResult =
        { original: string
          nextWord: string
          changes: string list
          errors: string list
          totalTime: float }

    let private transformWord (rulesFile: RulesFile) word =
        let rec inner syllables nextWord changes totalTime errors rules =
            match rules with
            | [] ->
                { original = word
                  nextWord = nextWord
                  changes = changes
                  errors = errors
                  totalTime = totalTime }

            | (lineNumber, node, rule)::xs ->
                let (result, locations), elapsed = time (fun () ->
                    if rulesFile.debug then
                        printfn "%s" DIVIDER
                        printfn "%d: %O" lineNumber node
                        printfn "%s" DIVIDER
                    Transducer.transformWithChangeLocations rulesFile.debug syllables rule nextWord)

                let (segmentLocations, segmentedWord), errors, syllableBoundaryTime =
                    let ruleLine = Node.getLine node

                    if result <> nextWord then
                        // If the word changed, get the new syllable boundaries
                        match time (fun () -> syllabizeWord rulesFile.debug rulesFile.syllableRules ruleLine result) with
                        | Ok ((_, segmentedWord) as result), ms ->
                            if segmentedWord <> "" then
                                System.Diagnostics.Debug.WriteLine(">>>>" + segmentedWord);

                            result, errors, ms
                        | Error message, ms ->
                            (Map.empty, ""), (message :: errors), ms
                    else
                        syllables, errors, 0.0

                let changes =
                    if result = nextWord then
                        changes
                    else
                        let changeLine =
                            if rulesFile.format = IPA then
                                Transducer.getIpaChangeLine lineNumber locations result
                            else
                                Transducer.getXsampaChangeLine lineNumber locations result
                        changes @ changeLine

                inner (segmentLocations, segmentedWord) result changes (totalTime + elapsed + syllableBoundaryTime) errors xs

        let initialSyllables =
            syllabizeWord rulesFile.debug rulesFile.syllableRules rulesFile.lineNumbers[0] word
            |> Result.defaultValue (Map.empty, "")

        (rulesFile.lineNumbers, rulesFile.ruleNodes, rulesFile.rules)
        |||> List.zip3
        |> inner initialSyllables word [] 0.0 []
    
    let transformLexicon rulesFile lexicon =
        let inline transformSerial () = lexicon |> Array.map (fun word -> transformWord rulesFile word)
        let inline transformParallel () = lexicon |> Array.Parallel.map (fun word -> transformWord rulesFile word)

    #if DEBUG
        let fTransform = transformSerial
    #else
        let fTransform =
            if rulesFile.debug && Array.length lexicon > 1 then
                transformSerial
            else
                transformParallel
    #endif

        time fTransform

    let dumpRules rules =
        for i, (node, rule) in rules do
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
