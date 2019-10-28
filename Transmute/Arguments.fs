module Arguments

type Options =
    { lexiconFile: string;
      rulesFile: string
    }

let defaultOptions =
    { lexiconFile = null;
      rulesFile = null
    }

let rec parseInternal (args: string list) (options: Options) =
    match args with
    | [] ->
        options
    | "--lexicon"::xs ->
        let nextOptions = { options with lexiconFile = args.[1] }
        parseInternal args.[2..] nextOptions
    | "--rules"::xs ->
        let nextOptions = { options with rulesFile = args.[1] }
        parseInternal args.[2..] nextOptions
    | x::xs ->
        eprintfn "Option '%s' is unrecognized" x
        parseInternal xs options


let parse (argv: string[]) =
    parseInternal (Array.toList argv) defaultOptions
