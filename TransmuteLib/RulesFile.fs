// Project:     Transmute
// Module:      RulesFile
// Description: Functions for loading rules files and using them to transform lexicons.
// Copyright:   (c) 2026 Matt Arriola
// License:     MIT

namespace TransmuteLib

open System
open System.IO
open System.Collections.Concurrent

type RulesFile =
    { format: InputFormat
      rules: CompileRuleResult list
      syllableRules: SyllableDefinitionRule list
      defaultChangeMarker: ChangeMarker
      totalCompileTime: float
      cached: bool
      debug: bool }

and CompileRuleResult =
    { lineNumber: int
      compileTime: float
      node: Node
      compiledRule: RuleCompiler.SoundChangeRule }

and SyllableDefinitionRule = int * (string -> string)

and ChangeMarker = NoChangeMarker | Underline | DoubleUnderline | Caret | Ascii

type RulesFileOptions =
    { debug: bool
      cached: bool
      recompile: bool
      silent: bool
      showNfa: bool
      parallelism: int
      testRules: int list option
      format: InputFormat
      source: Stream
      onProgress: OnProgress
      onComplete: OnComplete }
with
    static member Default =
        { debug = false
          cached = false
          recompile = true
          silent = true
          showNfa = false
#if FABLE_COMPILER
          parallelism = 4
#else
          parallelism = System.Environment.ProcessorCount
#endif
          testRules = None
          format = IPA
          source = Console.OpenStandardInput()
          onProgress = fun _ _ -> ()
          onComplete = fun () -> () }

    static member inline fromText (text: string) options = { options with source = new MemoryStream(System.Text.Encoding.UTF8.GetBytes text) }

    static member inline withDebug b options = { options with debug = b }

    static member inline withCache b options = { options with cached = b }

    static member inline withRecompile b options = { options with recompile = b }

    static member inline withSilence b options = { options with silent = b }

    static member inline withNfaDump b options = { options with showNfa = b }

    static member inline withParallelism value options = { options with parallelism = value }

    static member inline withTestRules ruleNumbers options = { options with testRules = ruleNumbers }

    static member inline withInputFormat format options = { options with format = format }

    static member inline withSource stream options = { options with source = stream }

    static member inline withProgressHook fProgress options = { options with onProgress = fProgress }

    static member inline withProgressAction (onProgress: Action<int, int>) options = { options with onProgress = fun current max -> onProgress.Invoke(current, max) }

    member this.FromText text = RulesFileOptions.fromText text this

    member this.WithDebug b = RulesFileOptions.withDebug b this

    member this.WithCache b = RulesFileOptions.withCache b this

    member this.WithRecompile b = RulesFileOptions.withRecompile b this

    member this.WithSilence b = RulesFileOptions.withSilence b this

    member this.WithParallelism n = RulesFileOptions.withParallelism n this

    member this.WithInputFormat inputFormat = RulesFileOptions.withInputFormat inputFormat this

    member this.WithProgressHook f = RulesFileOptions.withProgressAction f this

and OnProgress = int -> int -> unit
and OnComplete = unit -> unit

type TransformResult =
    { original: string
      nextWord: string
      changes: LexiconChange list
      errors: (int * string) list
      totalTime: float }
and LexiconChange =
    { isOriginal: bool
      ruleNumber: int
      change: string }

module private RulesFile =
    let private DIVIDER = new System.String('-', 80)

    let inline time fn =
        let start = DateTime.Now
        let result = fn()
        let stop = DateTime.Now
        let milliseconds = (stop - start).TotalMilliseconds
        result, milliseconds

    /// Estimates the complexity of a rule by counting the number of times sets or features are matched.
    let countSets (RuleNode (_, _, input, _, environment)) =
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

    let compileRules options features sets syllableDefinitions rules =
        let syllableRules =
            syllableDefinitions
            |> List.map (fun (SyllableDefinitionNode (lineNumber, _, _, _) as node) -> lineNumber, SyllableRuleCompiler.compile features sets node)

        let progressQueue = new BlockingCollection<bool>()

        let advanceProgress() = progressQueue.Add true
        let completeProgress() = progressQueue.Add false
        let ruleCount = List.length rules

        async {
            let mutable doContinue = true
            let mutable completedCount = 0
            while doContinue do
                doContinue <- progressQueue.Take()
                if doContinue then
                    completedCount <- completedCount + 1
                    options.onProgress completedCount ruleCount
            ()
        }
        |> Async.Start

#if DEBUG
        let parallelism = 1
        let ruleBins = [ rules ]
#else
        // Sort rules in descending order of estimated complexity and distribute the compilation across each processor core

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
                    advanceProgress()
                compiledRules.Add
                    { lineNumber = Node.getLine node
                      compileTime = elapsed
                      node = node
                      compiledRule = rule }
                compileBin rest

        let opts = new System.Threading.Tasks.ParallelOptions(MaxDegreeOfParallelism = parallelism)
        let _, rulesTime = time (fun () -> System.Threading.Tasks.Parallel.ForEach(ruleBins, opts, compileBin))
        completeProgress()

        let rules =
            compiledRules
            |> Seq.sortBy (fun result -> result.lineNumber)
            |> Seq.toList

        syllableRules, rules, rulesTime

    let loadInternal options =
        (new StreamReader(options.source)).ReadToEnd()
        |> RuleParser.parse options.format
        |> Result.bind (fun (ParseResult (sets, features, syllableDefinitions, rules)) ->
            let selectedRules =
                match options.testRules with
                | None -> rules
                | Some testRules ->
                    rules
                    |> List.filter (function RuleNode (lineNumber, _, _, _, _) -> List.contains lineNumber testRules)

            let syllableRules, rules, compileTime = compileRules options features sets syllableDefinitions selectedRules

            Ok { format = options.format
                 cached = false
                 debug = options.debug
                 defaultChangeMarker =
                    match options.format with
                    | IPA -> Underline
                    | X_SAMPA -> Ascii
                 totalCompileTime = compileTime
                 syllableRules = syllableRules
                 rules = rules })

    /// <summary>
    /// Selects the syllable rule that applies to the given line in the rule set, and detects syllable boundaries and segment locations in the given word.
    /// </summary>
    /// <remark>
    /// When multiple syllable definitions occur in a rule set, each applies to all following rules up to the next syllable definition node, which
    /// replaces the last one from that point on.
    /// </remark>
    let syllabizeWord debug syllableRules (ruleLine: int) word =
#if VERBOSE
        System.Diagnostics.Debug.WriteLine DIVIDER
        System.Diagnostics.Debug.WriteLine "Syllabizer"
        System.Diagnostics.Debug.WriteLine DIVIDER
#endif

        if Seq.isEmpty syllableRules then
            Result.Ok String.Empty
        else
            let results =
                syllableRules
                |> Seq.filter (fst >> (>) ruleLine)
                |> Seq.rev
                |> Seq.map (fun (_, syllableRule) ->
#if VERBOSE
                    System.Diagnostics.Debug.WriteLine (">>>> " + word)
#endif
                    SyllableBoundaryDetector.findBoundaries syllableRule word)

            let errorResult =
                results
                |> Seq.tryFind Result.isError
                |> Option.defaultValue (Result.Ok String.Empty)
            
            results
            |> Seq.tryFind Result.isOk
            |> Option.defaultValue errorResult

    let markChangesWithCombining diacritic nonCombiningDiacritic ruleNum (changes: int list) (result: string) =
        let result = List.ofSeq result
    
        // Insert a combining long underline after each change, but if there's a deletion at the end, put a regular underscore there
        let deletionAtEnd, changes =
            changes
            |> List.rev
            |> List.partition ((<=) result.Length)
        let outChars =
            let chars =
                (result, changes)
                ||> List.fold (fun result location -> List.insertAt (location + 1) diacritic result)
            if List.isEmpty deletionAtEnd then
                chars
            else
                chars @ [nonCombiningDiacritic]

        let out = System.String.Join("", outChars)

        [ { isOriginal = false; ruleNumber = ruleNum; change = out } ]

    let markChangesWithAsciiCarets ruleNum (changes: int list) (result: string) = 
        let maxIndex = List.max changes + 1

        let changeLine =
            Array.create maxIndex ' '
            |> Array.mapi (fun i _ -> if List.contains i changes then "^" else " ")
            |> String.concat ""

        [ { isOriginal = false; ruleNumber = ruleNum; change = result }
          { isOriginal = false; ruleNumber = -1; change = changeLine } ]

    let don'tMarkChanges ruleNumber _ s =
        [ { isOriginal = false; ruleNumber = ruleNumber; change = s } ]

    let transformWord (rulesFile: RulesFile) fMarkChanges word =
        let rec inner syllables nextWord changes totalTime errors rules =
            match rules with
            | [] ->
                { original = word
                  nextWord = nextWord
                  changes = [ { isOriginal = true; ruleNumber = -1; change = word } ] @ changes
                  errors = errors
                  totalTime = totalTime }

            | { lineNumber = lineNumber; node = node; compiledRule = rule }::xs ->
                let (result, locations), transformTime = time (fun () ->
                    if rulesFile.debug then
                        printfn "%s" DIVIDER
                        printfn "%d: %O" lineNumber node
                        printfn "%s" DIVIDER
                    Transducer.transformWithChangeLocations rulesFile.debug syllables rule nextWord)

                let segmentedWord, errors, syllableBoundaryTime =
                    let ruleLine = Node.getLine node

                    if result = nextWord then
                        syllables, errors, 0.0
                    else
                        // If the word changed, get the new syllable boundaries

                        match time (fun () -> syllabizeWord rulesFile.debug rulesFile.syllableRules ruleLine result) with
                        | Ok segmentedWord, ms ->
#if VERBOSE
                            if segmentedWord <> "" then
                                System.Diagnostics.Debug.WriteLine(">>>>" + segmentedWord);
#endif

                            segmentedWord, errors, ms
                        | Error message, ms ->
                            String.Empty, ((lineNumber, message) :: errors), ms

                let changes =
                    if result = nextWord then
                        changes
                    else
                        changes @ fMarkChanges lineNumber locations result

                inner segmentedWord result changes (totalTime + transformTime + syllableBoundaryTime) errors xs

        let initialSyllables =
            word
            |> syllabizeWord rulesFile.debug rulesFile.syllableRules rulesFile.rules[0].lineNumber
            |> Result.defaultValue String.Empty

        inner initialSyllables word [] 0.0 [] rulesFile.rules

type RulesFile with
    [<CompiledName("Load")>]
    static member load options =
        async {
            return RulesFile.loadInternal options
        }

    [<CompiledName("LoadAsync")>]
    static member loadAsync options = Async.StartAsTask (RulesFile.load options)

    [<CompiledName("TransformLexicon")>]
    static member transformLexicon rulesFile changeMarker lexicon =
        let fMarkChanges =
            match changeMarker with
            | Some Underline -> RulesFile.markChangesWithCombining '\u0332' '_'
            | Some DoubleUnderline -> RulesFile.markChangesWithCombining '\u0333' '_'
            | Some Caret -> RulesFile.markChangesWithCombining '\u032d' '\u02f0'
            | Some Ascii -> RulesFile.markChangesWithAsciiCarets
            | _ -> RulesFile.don'tMarkChanges

        let inline transformSerial () = lexicon |> Array.map (fun word -> RulesFile.transformWord rulesFile fMarkChanges word)
        let inline transformParallel () = lexicon |> Array.Parallel.map (fun word -> RulesFile.transformWord rulesFile fMarkChanges word)

    #if DEBUG
        let fTransform = transformSerial
    #else
        let fTransform =
            if rulesFile.debug && Array.length lexicon > 1 then
                transformSerial
            else
                transformParallel
    #endif

        RulesFile.time fTransform

    [<CompiledName("ApplyRules")>]
    member this.applyRules (lexicon: System.Collections.Generic.IEnumerable<string>, ?changeMarker: ChangeMarker) =
        async {
            let lexicon = Seq.toArray lexicon
            return (RulesFile.transformLexicon this changeMarker lexicon)
        }
        |> Async.StartAsTask
