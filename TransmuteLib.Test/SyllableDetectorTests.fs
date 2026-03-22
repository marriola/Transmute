// Project:     TransmuteLib.Test
// Module:      Syllable boundary detection tests
// Copyright:   (c) 2026 Matt Arriola
// License:     MIT

module TransmuteLib.Test.SyllableDetectorTests

open TransmuteLib
open Xunit

[<Fact>]
let ``Categorizing segments works`` () =
    let input = "tsɛrbʏrstən"
    let expected = ".OONCC.NCCC.NC"
    let rule =
        """
        C = (k, p, t, g, b, d, s, z, ts, pf, ʃ, f, v, x, r, l, m, n, ŋ)
        V = (a, ɛ, ɪ, ɔ, ʊ, ʏ, œ, aː, eː, iː, oː, uː, yː, øː, ə)

        Syllable = (
            Onset = (C)(C)(C)
            Nucleus = V
            Coda = (C)(C)(C)
        )
        """
    let (syllableRules, features, sets, _) =
        rule
        |> RuleParser.parseRules InputFormat.IPA
        |> Result.toOption
        |> Option.get
    let syllableRule = SyllableRuleCompiler.compile features sets (snd syllableRules[0])
    let actual = syllableRule input
    Assert.Equal<string>(expected, actual)

[<Fact>]
let ``Syllable boundary detected at the beginning`` () =
    let input = ["akka"]
    let expected = ["ɐkka"]
    let rule =
        """
        Syllable = (
            Onset = (C)
            Nucleus = V
            Coda = (C)
        )

        C = (k, p, t, s, m, n, ʔ)
        V = (a, ɐ, e, i, o, u)

        a → ɐ / σ_
        """
    Tests.testRules IPA rule input expected

[<Fact>]
let ``Syllable boundary detected at the end`` () =
    let input = ["tatai"]
    let expected = ["tɐtai"]
    let rule =
        """
        Syllable = (
            Onset = (C)
            Nucleus = V(V)
            Coda = (C)
        )

        C = (k, p, t, s, m, n, ʔ)
        V = (a, ɐ, e, i, o, u)

        a → ɐ / _σ
        """
    Tests.testRules IPA rule input expected


[<Fact>]
let ``Change conditioned by presence of two following syllables`` () =
    let input = ["papa"; "kitiki"]
    let expected = ["papa"; "ktiki"]
    let rule =
        """
        Syllable = (
            Onset = (C)(C)
            Nucleus = V
            Coda = (C)(C)
        )

        V = (a, e, i, o, u)
        C = (p, t, k)

        V → ∅ / _$$$
        """
    Tests.testRules IPA rule input expected
