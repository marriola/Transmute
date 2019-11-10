module Arguments

type Options =
    { lexiconFile: string;
      rulesFile: string
      testRules: int list option
    }

let defaultOptions =
    { lexiconFile = null
      rulesFile = null
      testRules = None
    }

let rec parseInternal (args: string list) (options: Options) =
    match args with
    | [] ->
        options
    | "--lexicon"::filename::xs ->
        let nextOptions = { options with lexiconFile = filename }
        parseInternal xs nextOptions
    | "--rules"::filename::xs ->
        let nextOptions = { options with rulesFile = filename }
        parseInternal xs nextOptions
    | "--test"::ruleNumbers::xs ->
        let nextOptions =
            { options with
                testRules =
                    ruleNumbers.Split(',')
                    |> Array.map (fun s -> s.Trim())
                    |> Array.map int
                    |> List.ofArray
                    |> Some
            }
        parseInternal xs nextOptions
    | x::xs ->
        eprintfn "Option '%s' is unrecognized" x
        parseInternal xs options


let parse (argv: string[]) =
    parseInternal (Array.toList argv) defaultOptions
