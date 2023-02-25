﻿module Arguments

type VerbosityLevel =
    | Silent
    | Normal
    | ShowTransformations
    | ShowDFA
    | ShowNFA

let verbosityLevels = dict [
    "0", Silent
    "1", Normal
    "2", ShowTransformations
    "3", ShowDFA
    "4", ShowNFA
]

type Options =
    { lexiconFiles: string list
      rulesFile: string
      testRules: int list option
      testWords: int list option
      verbosityLevel: VerbosityLevel
      recompile: bool
    }

let defaultOptions =
    { lexiconFiles = []
      rulesFile = null
      testRules = None
      testWords = None
      verbosityLevel = Normal
      recompile = false
    }

let parse (argv: string[]) =
    let rec parse' args options =
        match args with
        | [] ->
            { options with lexiconFiles = List.rev options.lexiconFiles }
        | "-l"::filename::xs
        | "--lexicon"::filename::xs ->
            parse' xs { options with lexiconFiles = filename :: options.lexiconFiles }
        | "-r"::filename::xs
        | "--rules"::filename::xs ->
            parse' xs { options with rulesFile = filename }
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
        | "-v"::x::xs when verbosityLevels.ContainsKey x ->
            parse' xs { options with verbosityLevel = verbosityLevels[x] }
        // Alias for -v 2
        | "--show-transformations"::xs ->
            parse' xs { options with verbosityLevel = ShowTransformations }
        // Alias for -v 3
        | "--verbose"::xs
        | "-v"::xs ->
            parse' xs { options with verbosityLevel = ShowDFA }
        | "-rc"::xs
        | "--recompile"::xs ->
            parse' xs { options with recompile = true }
        | filename::xs when filename.EndsWith ".sc" ->
            parse' xs { options with rulesFile = filename }
        | filename::xs ->
            parse' xs { options with lexiconFiles = filename :: options.lexiconFiles }

    parse' (Array.toList argv) defaultOptions

let validate (options: Options) =
    let mutable isValid = true
    if options.lexiconFiles.Length = 0 then
        printfn "Lexicon not specified"
        isValid <- false
    if options.rulesFile = null then
        printfn "Rules file not specified"
        isValid <- false
    isValid
