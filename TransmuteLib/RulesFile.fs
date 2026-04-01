// Project:     Transmute
// Module:      RulesFile
// Description: Functions for loading rules files and using them to transform lexicons.
// Copyright:   (c) 2026 Matt Arriola
// License:     MIT

namespace TransmuteLib

open System
open System.IO
open System.Collections.Concurrent

open TransmuteLib.Utils.Operators

type SyllableDefinitionRule = int * (string -> string)

type CompileRuleResult =
    { lineNumber: int
      compileTime: float
      node: Node
      compiledRule: RuleCompiler.CompiledRule }

module RulesFile =
    type Options =
        { debug: bool
          cached: bool
          recompile: bool
          silent: bool
          showNfa: bool
          parallelism: int
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
              parallelism = System.Environment.ProcessorCount
              testRules = None
              format = IPA
              source = Console.OpenStandardInput() }

        static member withDebug b options = { options with debug = b }
        static member withCache b options = { options with cached = b }
        static member withRecompile b options = { options with recompile = b }
        static member withSilent b options = { options with silent = b }
        static member withNfaDump b options = { options with showNfa = b }
        static member withParallelism value options = { options with parallelism = value }
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
          rules: CompileRuleResult list
          syllableRules: SyllableDefinitionRule list
          totalCompileTime: float
          recompiled: bool
          debug: bool }

    let inline private time fn =
        let start = DateTime.Now
        let result = fn()
        let stop = DateTime.Now
        let milliseconds = (stop - start).TotalMilliseconds
        result, milliseconds

    /// Estimates the complexity of a rule by counting the number of times sets or features are used.
    let private countSets (RuleNode (_, input, _, environment)) =
        let rec inner nodes cost =
            match nodes with
            | [] ->
                cost
            | PlaceholderNode :: rest
            | UtteranceNode _ :: rest
            | WordBoundaryNode :: rest
            | SyllableBoundaryNode _ :: rest ->
                inner rest cost
            | CompoundSetIdentifierNode _ :: rest
            | SetIdentifierNode _ :: rest ->
                inner rest (cost + 1)
            | NegationNode node :: rest ->
                inner rest (inner [node] 1 + cost)
            | OptionalNode optionalNodes :: rest ->
                inner rest (inner optionalNodes 1 + cost)
            | AlternationNode alternations :: rest ->
                let avgCost =
                    alternations
                    |> List.map (fun branch -> inner branch 1 |> float)
                    |> List.average
                    |> int
                inner rest (avgCost + cost)
            | x :: _ ->
                failwithf "%O" x

        inner input 0
        |> inner environment

    let private startProgress options max =
        let progressQueue = new BlockingCollection<bool>()

        if not options.silent then
            let ruleCount = max
            let numDigits = ruleCount |> Math.Log10 |> Math.Ceiling |> int
            let spacing = numDigits * 2 + 1

            Console.Error.Write (String.Format("[{0}] Compiling...\r", String.replicate spacing " "))

            async {
                let mutable doContinue = true
                let mutable completedCount = 0
                while doContinue do
                    doContinue <- progressQueue.Take()
                    if doContinue then
                        completedCount <- completedCount + 1
                        let progress = $"{completedCount}/{ruleCount}".PadLeft spacing
                        Console.Error.Write $"[{progress}]\r"
                ()
            }
            |> Async.Start

        let addProgress() = progressQueue.Add true
        let completeProgress() = progressQueue.Add false

        addProgress, completeProgress

    let private compileRules options features sets syllableDefinitions rules =
        let syllableRules =
            syllableDefinitions
            |> List.map (fun (i, node) -> i, SyllableRuleCompiler.compile features sets node)

        let addProgress, completeProgress = startProgress options (List.length rules)

        // Sort rules in descending order of estimated complexity and distribute the compilation across each processor core

#if DEBUG
        let parallelism = 1
        let ruleBins = [ rules ]
#else
        let parallelism = options.parallelism

        let ruleBins =
            rules
            |> List.sortByDescending countSets
            |> List.map Some
            |> List.chunkBySize parallelism
            |> List.map (fun chunk ->
                // Make sure every chunk is the same size to keep List.transpose happy
                let difference = parallelism - chunk.Length
                if difference > 0
                    then chunk @ List.replicate difference None
                    else chunk)
            |> List.transpose
            |> List.map (List.choose id)
#endif

        let compiledRules = new ConcurrentBag<CompileRuleResult>()

        let rec compileBin = function
            | [] -> ()
            | node :: rest ->
                let rule, elapsed = time (fun () -> RuleCompiler.compile options.showNfa features sets node)
                if not options.silent then
                    addProgress()
                compiledRules.Add
                    { lineNumber = Node.getLine node
                      compileTime = elapsed
                      node = node
                      compiledRule = rule }
                compileBin rest

        let opts = new System.Threading.Tasks.ParallelOptions(MaxDegreeOfParallelism = parallelism)
        let _, rulesTime = time (fun () -> System.Threading.Tasks.Parallel.ForEach(ruleBins, opts, compileBin))

        if not options.silent then
            completeProgress()
            Console.Error.WriteLine "\n"

        let rules =
            compiledRules
            |> Seq.sortBy (fun result -> result.lineNumber)
            |> Seq.toList

        syllableRules, rules, rulesTime

    let load options =
        if options.recompile then
            let text = (new StreamReader(options.source)).ReadToEnd()
            let parseResult = RuleParser.parseRules options.format text
            let syllableDefinitions, features, sets, rules = Result.orAbort parseResult

            let selectedRules =
                match options.testRules with
                | None -> rules
                | Some testRules ->
                    rules
                    |> List.filter (function RuleNode (lineNumber, _, _, _) -> List.contains lineNumber testRules)

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
              syllableRules = syllableRules
              rules = rules }
        else
            fprintfn stderr "Loading rules..."
            let (syllableRules, rules), loadTime =
                time (fun () -> RuleCompiler.readCompiledRulesFromStream options.source)

            { format = options.format
              recompiled = false
              debug = options.debug
              totalCompileTime = loadTime
              syllableRules = syllableRules
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
          errors: (int * string) list
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

            | { lineNumber = lineNumber; node = node; compiledRule = rule }::xs ->
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
                            (Map.empty, ""), ((lineNumber, message) :: errors), ms
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
            syllabizeWord rulesFile.debug rulesFile.syllableRules rulesFile.rules[0].lineNumber word
            |> Result.defaultValue (Map.empty, "")

        inner initialSyllables word [] 0.0 [] rulesFile.rules
    
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
