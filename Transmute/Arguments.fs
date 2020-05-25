module Arguments

type VerbosityLevel =
    | Silent
    | ShowTransformations
    | ShowDFA
    | ShowNFA
    with
        member this.verbose () =
            match this with
            | Silent
            | ShowTransformations -> false
            | _ -> true

        member this.showTransformations () =
            match this with
            | Silent -> false
            | _ -> true

        member this.showNfa () =
            match this with
            | ShowNFA -> true
            | _ -> false

let verbosityLevels = dict [
    "0", Silent
    "1", ShowTransformations
    "2", ShowDFA
    "3", ShowNFA
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
      verbosityLevel = Silent
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
        | "--verbose"::xs ->
            parse' xs { options with verbosityLevel = ShowDFA }
        | "--show-transformations"::xs ->
            parse' xs { options with verbosityLevel = ShowTransformations }
        | x::xs ->
            eprintfn "Option '%s' is unrecognized" x
            parse' xs options
    parse' (Array.toList argv) defaultOptions
