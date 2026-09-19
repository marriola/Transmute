// Project:     TransmuteLib.Test
// Module:      Tests
// Copyright:   (c) 2026 Matt Arriola
// License:     MIT

module TransmuteLib.Test.Tests

open TransmuteLib.Utils.Operators
open TransmuteLib
open Xunit

let testRules format rulesText lexicon expected =
    let rulesFile =
        RulesFileOptions.Default
        |> RulesFileOptions.fromText rulesText
        |> RulesFileOptions.withInputFormat format
        |> RulesFile.load
        |> Async.RunSynchronously
        |> Result.orThrow

    let actual =
        lexicon
        |> List.toArray
        |> RulesFile.transformLexicon rulesFile None
        |> fst
        |> Seq.map (fun result -> result.nextWord)
        |> Seq.toList
    
    Assert.Equal<string list>(expected, actual)
