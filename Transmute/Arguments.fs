// Project:     Transmute
// Module:      Arguments
// Description: Console driver command line argument parser
// Copyright:   (c) 2023 Matt Arriola
// License:     MIT

module Arguments

open TransmuteLib
open System.IO

type VerbosityLevel =
    | Silent
    | Normal
    | ShowTransformations
    | ShowTimes
    | ShowDFA
    | ShowNFA

let verbosityLevels = dict [
    "0", Silent
    "1", Normal
    "2", ShowTransformations
    "3", ShowTimes
    "4", ShowDFA
    "5", ShowNFA
]

type Options =
    { format: InputFormat
      lexiconFiles: InputSource list
      rulesFile: InputSource
      outputFile: string option
      recompile: bool
      listRules: bool
      saveRules: bool
      debug: bool
      debugSyllabizer: bool
      verbosityLevel: VerbosityLevel
      testRules: int list option
      testWords: int list option
      parallelism: int }

and SourceFile =
    | RulesFileSource of string
    | LexiconFileSource of string

with
    static member private RULES_EXTENSION = ".sc"
    static member private COMPILED_RULES_EXTENSION = ".scc"

    member this.CompiledRuleFilePath
        with get() =
            match this with
            | RulesFileSource path ->
                if Path.GetExtension path = SourceFile.RULES_EXTENSION then
                    Path.Combine(Path.GetDirectoryName path, Path.GetFileNameWithoutExtension path) + SourceFile.COMPILED_RULES_EXTENSION
                else
                    path + SourceFile.COMPILED_RULES_EXTENSION

            | LexiconFileSource _ ->
                invalidArg "path" "Path is for a lexicon file"

and InputSource =
    | NotSpecified
    | StandardInput
    | File of SourceFile

with
    member this.GetStream () =
        match this with
        | NotSpecified ->
            failwith "Input source not specified"

        | StandardInput ->
            System.Console.OpenStandardInput()

        | File (RulesFileSource path)
        | File (LexiconFileSource path) ->
            new System.IO.FileStream(path, FileMode.Open, FileAccess.Read, FileShare.ReadWrite) :> Stream

    member this.Read () =
        match this with
        | NotSpecified ->
            failwith "Input source not specified"

        | StandardInput ->
            System.Console.In.ReadToEnd().Replace("\r", "\n")

        | File (RulesFileSource path)
        | File (LexiconFileSource path) ->
            (new System.IO.StreamReader(path)).ReadToEnd().Trim()

    member this.Fresh
        with get() =
            match this with
            | NotSpecified ->
                failwith "Input source not specified"

            | StandardInput
            | File (LexiconFileSource _) ->
                true

            | File (RulesFileSource path as rulesFile) ->
                not (File.Exists path) || File.GetLastWriteTime path > File.GetLastWriteTime rulesFile.CompiledRuleFilePath

let defaultOptions =
    { format = IPA
      lexiconFiles = []
      rulesFile = NotSpecified
      outputFile = None
      recompile = false
      listRules = false
      saveRules = true
      debug = false
      debugSyllabizer = false
      verbosityLevel = Normal
      testRules = None
      testWords = None
      parallelism = System.Environment.ProcessorCount }

let parse (argv: string[]) =
    let rec parse' args options =
        match args with
        | [] ->
            { options with lexiconFiles = List.rev options.lexiconFiles }

        | "-l"::filename::xs
        | "--lexicon"::filename::xs ->
            parse' xs { options with lexiconFiles = File (LexiconFileSource filename) :: options.lexiconFiles }

        | "-lr"::xs
        | "--list-rules"::xs ->
            parse' xs { options with listRules = true }

        | "-ns"::xs
        | "--no-save"::xs ->
            parse' xs { options with saveRules = false }

        | "-o"::path::xs
        | "--out"::path::xs ->
            parse' xs { options with outputFile = Some path }

        | "-r"::filename::xs
        | "--rules"::filename::xs ->
            parse' xs { options with rulesFile = File (RulesFileSource filename) }

        | "--test-rules"::ruleNumbers::xs ->
            let nextOptions =
                { options with
                    testRules =
                        ruleNumbers.Split(',')
                        |> Array.map (fun s -> s.Trim() |> int)
                            |> List.ofArray
                        |> Some
                }
            parse' xs nextOptions

        | "--test-words"::wordNumbers::xs ->
            let nextOptions =
                { options with
                    testWords =
                        wordNumbers.Split(',')
                        |> Array.map (fun s -> s.Trim() |> int)
                        |> List.ofArray
                        |> Some
                }
            parse' xs nextOptions

        | "-v"::x::xs ->
            if not (verbosityLevels.ContainsKey x) then
                failwith $"Invalid verbosity level: {x}"

            parse' xs { options with verbosityLevel = verbosityLevels[x] }

        // Alias for -v 2
        | "--show-transformations"::xs ->
            parse' xs { options with verbosityLevel = ShowTransformations }

        // Alias for -v 3
        | "--verbose"::xs
        | "-v"::xs ->
            parse' xs { options with verbosityLevel = ShowDFA }

        | "--debug"::xs
        | "-d"::xs ->
            parse' xs { options with debug = true }

        | "--debug-syllabizer"::xs
        | "-ds"::xs ->
            parse' xs { options with debug = true; debugSyllabizer = true }

        | "-rc"::xs
        | "--recompile"::xs ->
            parse' xs { options with recompile = true }

        | "-x"::xs
        | "--x-sampa"::xs ->
            parse' xs { options with format = X_SAMPA }

        | "-p"::parallelism::xs
        | "--parallelism"::parallelism::xs ->
            let options =
                match System.Int32.TryParse parallelism with
                | true, n -> { options with parallelism = n }
                | false, _ -> options
            parse' xs options

        | filename::xs when filename.EndsWith ".sc" ->
            parse' xs { options with rulesFile = File (RulesFileSource filename) }

        | filename::xs ->
            parse' xs { options with lexiconFiles = File (LexiconFileSource filename) :: options.lexiconFiles }

    parse' (Array.toList argv) defaultOptions

let validate (options: Options) =
    let mutable isValid = true

    if options.lexiconFiles.Length = 0 then
        printfn "Lexicon not specified"
        isValid <- false
    
    if options.rulesFile = NotSpecified then
        printfn "Rules file not specified"
        isValid <- false

    isValid
