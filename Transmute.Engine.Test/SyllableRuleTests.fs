// Project:     Transmute.Engine.Test
// Module:      Syllable rule tests
// Copyright:   (c) 2026 Matt Arriola
// License:     MIT

module Transmute.Engine.Test.SyllableRuleTests

open Transmute.Engine
open Transmute.Engine.Combinators
open Xunit

[<Fact>]
let ``Categorizing segments works`` () =
    let input = "tsɛrbʏrstən"
    let expected = "OONCCNCCCNC"
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
    let result = RuleParser.parse InputFormat.IPA rule
    let syllableRule = SyllableRuleCompiler.compile result.features result.sets result.syllableRules[0]
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
let ``Detect additional syllables`` () =
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

        a → ɐ / _σσ
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

        V → ∅ / _σσσ
        """
    Tests.testRules IPA rule input expected

[<Fact>]
let ``Compound syllable rules work`` () =
    let input = ["bab"; "baː"; "ba"]
    let expected = ["bæb"; "bɔː"; "ba"]
    let rule =
        """
            ; Only allow a coda if the vowel is short

            Syllable = (
                Onset = (C)
                Nucleus = [V-Long]
                Coda = (C)
            ) or (
                Onset = (C)
                Nucleus = V
            )

            C = (b)
            V = (a, aː)

            [Long] = (a → aː)

            a → æ / _ Coda
            aː → ɔː / _ SyllableEnd
        """

    Tests.testRules IPA rule input expected
