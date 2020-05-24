module Arguments

type Options =
    { lexiconFile: string;
      rulesFile: string
      testRules: int list option
      testWords: int list option
      verbose: bool
      showTransformations: bool
    }

let defaultOptions =
    { lexiconFile = null
      rulesFile = null
      testRules = None
      testWords = None
      verbose = false
      showTransformations = false
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
        | "--verbose"::xs ->
            parse' xs { options with verbose = true }
        | "--show-transformations"::xs ->
            parse' xs { options with showTransformations = true }
        | x::xs ->
            eprintfn "Option '%s' is unrecognized" x
            parse' xs options
    parse' (Array.toList argv) defaultOptions
