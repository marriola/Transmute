// Project:     TransmuteLib.Test
// Module:      Tests
// Copyright:   (c) 2026 Matt Arriola
// License:     MIT

module TransmuteLib.Test.Tests

open TransmuteLib
open Xunit

let testRules format rulesText lexicon expected =
    let rulesFile =
        RulesFile.Options.Default
        |> RulesFile.Options.fromText rulesText
        |> RulesFile.Options.withInputFormat format
        |> RulesFile.load

    let actual =
        lexicon
        |> List.toArray
        |> RulesFile.transformLexicon rulesFile
        |> fst
        |> Seq.map (fun result -> result.nextWord)
        |> Seq.toList
    
    Assert.Equal<string list>(expected, actual)

[<Fact>]
let ``Unconditional single phone transformation`` () =
    testRules IPA "k → t" ["ka"] ["ta"]

[<Fact>]
let ``Unconditional set transformation`` () =
    let input = [ "ka"; "pa"; "ta" ]
    let expected = [ "ga"; "ba"; "da" ]
    let rule =
        """
        [Voiced] = {
            k → g
            p → b
            t → d
        }

        [-Voiced] → [+Voiced]
        """
    testRules IPA rule input expected

[<Fact>]
let ``Unconditional disjunction transformation`` () =
    let input = [ "ke"; "ko" ]
    let expected = [ "ka"; "ka" ]
    let rule =
        """
        (e|o)→a
        """
    testRules IPA rule input expected

[<Fact>]
let ``Unconditional set reverse transformation`` () =
    let input = [ "ga"; "ba"; "da" ]
    let expected = [ "ka"; "pa"; "ta" ]
    let rule =
        """
        [Voiced] = {
            k → g
            p → b
            t → d
        }

        [+Voiced] → [-Voiced]
        """
    testRules IPA rule input expected

[<Fact>]
let ``Deletion`` () =
    let input = ["ha"]
    let expected = ["a"]
    let rule = "h → ∅"
    testRules IPA rule input expected

[<Fact>]
let ``Insertion in the middle`` () =
    let input = ["it"]
    let expected = ["ist"]
    let rule = "∅ → s / i_t"
    testRules IPA rule input expected

[<Fact>]
let ``Insertion at the beginning before an utterance`` () =
    let input = ["t"]
    let expected = ["it"]
    let rule = "∅ → i / _t"
    testRules IPA rule input expected

[<Fact>]
let ``Insertion at the beginning before a set`` () =
    let input = ["t"]
    let expected = ["it"]
    let rule = """
    ∅ → i / _C
    C = (k, p, t)
    """
    testRules IPA rule input expected

[<Fact>]
let ``Insertion at the end after an utterance`` () =
    let input = ["i"]
    let expected = ["it"]
    let rule = "∅ → t / i_"
    testRules IPA rule input expected

[<Fact>]
let ``Insertion at the end after a set`` () =
    let input = ["i"]
    let expected = ["it"]
    let rule = """
    ∅ → t / V_
    V = (a, e, i, o, u)
    """
    testRules IPA rule input expected

[<Fact>]
let ``Addition and replacement after an optional node`` () =
    let input = ["gʰˈlχʷtom"]
    let expected = ["gʰˈulχʷtom"]
    let rule =
        """
        DENTAL = { t d dʰ θ ð s z }
        LABIAL = { m p b bʰ ɸ β }
        LABIOVELAR = { kʷ gʷ gʷʰ xʷ ɣʷ }
        VELAR = { k g gʰ x ɣ LABIOVELAR }
        SONORANT = { m n l r w j }
        LIQUID = { l r }
        GLIDE = { w j }
        NASAL = { m n }
        LARYNGEAL = { ʔ χ χʷ }
        SIBILANT = { s }
        C = { STOP DENTAL LABIAL VELAR SONORANT LIQUID GLIDE NASAL LARYNGEAL SIBILANT }
        LARYNGEAL = { ʔ χ χʷ }

        ∅ → u / (#|[C-LARYNGEAL])(ˈ) _ (m|n|l|r)(#|C)
        """
    testRules IPA rule input expected

[<Fact>]
let ``Disjunct node`` () =
    let input = [ "la"; "ra" ]
    let expected = [ "lo"; "ro" ]
    let rule = "a → o / (l|r)_"
    testRules IPA rule input expected

[<Fact>]
let ``Compound sets`` () =
    let input = ["ka"]
    let expected = ["a"]
    let rules =
        """
        STOP = { k p t g b d }
        [Fricative] = {
            k → x
            p → ɸ
            t → θ
            g → ɣ
            b → β
            d → ð
        }
        [Voiced] = {
            k → g
            p → b
            t → d
            x → ɣ
            ɸ → β
            θ → ð
        }

        [STOP-Voiced] → [+Voiced]                ; ka → ga
        [+Voiced-Fricative] → [+Fricative]       ; ga → ɣa
        ɣ → ∅                                    ; ɣa → a
        """
    testRules IPA rules input expected

[<Fact>]
let ``Multi-phoneme set transformations`` () =
    let input = [ "ta"; "twa"; "sa"; "da"; "dwa" ]
    let expected = [ "ta"; "ta"; "tsa"; "da"; "da" ]
    let rules =
        """
        [Labialized] = {
            t → tw
            d → dw
        }
        [Affricate] = {
            s → ts
        }

        [+Labialized] → [-Labialized]
        [-Affricate] → [+Affricate]
        """
    testRules IPA rules input expected

[<Fact>]
let ``Repeat simple replacement`` () =
    let input = ["kak"]
    let expected = ["xax"]
    let rule = "k → x"
    testRules IPA rule input expected

[<Fact>]
let ``Repeat set replacement`` () =
    let input = ["gʷakʷ"]
    let expected = ["ɣʷaxʷ"]
    let rule =
        """
        [Fricative] = {
            k → x
            kʷ → xʷ
            g → ɣ
            gʷ → ɣʷ
        }

        [-Fricative] → [+Fricative]
        """
    testRules IPA rule input expected

[<Fact>]
let ``X-SAMPA works`` () =
    let input = ["pater"]
    let expected = ["p\\aTer"]
    let rule =
        """
        [$Fricative] = (
            k -> x
            p -> p\
            t -> T
        )

        [-$Fricative] -> [+$Fricative]
        """
    testRules X_SAMPA rule input expected

[<Fact>]
let ``X-SAMPA diacritics work`` () =
    let input = ["k_walos"]
    let expected = ["x_walos"]
    let rule =
        """
        [$Fricative] = (
            k -> x
            k_w -> x_w
            p -> p\
            t -> T
        )

        [-$Fricative] -> [+$Fricative]
        """
    testRules X_SAMPA rule input expected
