module Arguments

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
    { lexiconFile: string;
      rulesFile: string
      testRules: int list option
      testWords: int list option
      verbosityLevel: VerbosityLevel
    }

let defaultOptions =
    { lexiconFile = null
      rulesFile = null
      testRules = None
      testWords = None
      verbosityLevel = Normal
    }

let parse (argv: string[]) =
    let rec parse' args options =
        match args with
        | [] ->
            options
        | "--lexicon"::filename::xs ->
            parse' xs { options with lexiconFile = filename }
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
        | "-v"::x::xs when verbosityLevels.ContainsKey(x) ->
            parse' xs { options with verbosityLevel = verbosityLevels.[x] }
        | "--verbose"::xs
        | "-v"::xs ->
            parse' xs { options with verbosityLevel = ShowDFA }
        | "--show-transformations"::xs ->
            parse' xs { options with verbosityLevel = ShowTransformations }
        | x::xs ->
            eprintfn "Unrecognized option '%s'" x
            parse' xs options

    parse' (Array.toList argv) defaultOptions
