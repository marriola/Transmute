// Project:     TransmuteLib.Test
// Module:      Tests
// Copyright:   (c) 2023 Matt Arriola
// License:     MIT

module TransmuteLib.Test.Tests

open TransmuteLib
open Xunit

let getLocations syllableRules word =
    syllableRules
    |> List.map (fun rule -> SyllableBoundaryDetector.get false rule word)
    |> List.filter (fun (xs, ys) -> List.length xs > 0)
    |> List.tryHead
    |> Option.defaultValue ([], Map.empty)

let testRule format rules inputs expected =
    let actual =
        rules
        |> RuleParser.parseRules format
        |> Result.map (fun (syllableDefinitions, features, sets, rules) ->
            let syllableRules = [] //syllableDefinitions |> List.map (fun (_, node) -> RuleCompiler.compileSyllableRule false features sets node)
            let getSyllableBoundaries = getLocations syllableRules
            let rule = RuleCompiler.compileRule false features sets rules[0]
            getSyllableBoundaries, rule)
        |> Result.map (fun (getBoundaries, rule) ->
            inputs |> List.map (fun word -> Transducer.transform false (getBoundaries word) rule word))
    Assert.Equal<string>(expected, Result.defaultWith List.singleton actual) 

let testRules format rules inputs (expected: string list) =
    let rules =
        rules
        |> RuleParser.parseRules format
        |> Result.map (fun (syllableDefinitions, features, sets, rules) ->
            let syllableRules = [] //syllableDefinitions |> List.map (fun (_, node) -> RuleCompiler.compileSyllableRule false features sets node)
            let getSyllableBoundaries = getLocations syllableRules
            let rules = RuleCompiler.compileRulesParallel false features sets rules
            getSyllableBoundaries, rules)
    let transform input =
        rules
        |> Result.map (fun (getSyllableBoundaries, rules) ->
            (input, rules)
            ||> List.fold (fun input rule -> Transducer.transform false (getSyllableBoundaries input) rule input))
    let actual =
        inputs
        |> List.map transform
    Assert.Equal<string list>(expected, List.map (Result.defaultWith id) actual)

[<Fact>]
let ``Unconditional single phone transformation`` () =
    testRule IPA "k → t" ["ka"] ["ta"]

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
    testRule IPA rule input expected

[<Fact>]
let ``Unconditional disjunction transformation`` () =
    let input = [ "ke"; "ko" ]
    let expected = [ "ka"; "ka" ]
    let rule =
        """
        (e|o)→a
        """
    testRule IPA rule input expected

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
    testRule IPA rule input expected

[<Fact>]
let ``Deletion`` () =
    let input = ["ha"]
    let expected = ["a"]
    let rule = "h → ∅"
    testRule IPA rule input expected

[<Fact>]
let ``Insertion in the middle`` () =
    let input = ["it"]
    let expected = ["ist"]
    let rule = "∅ → s / i_t"
    testRule IPA rule input expected

[<Fact>]
let ``Insertion at the beginning before an utterance`` () =
    let input = ["t"]
    let expected = ["it"]
    let rule = "∅ → i / _t"
    testRule IPA rule input expected

[<Fact>]
let ``Insertion at the beginning before a set`` () =
    let input = ["t"]
    let expected = ["it"]
    let rule = """
    ∅ → i / _C
    C = (k, p, t)
    """
    testRule IPA rule input expected

[<Fact>]
let ``Insertion at the end after an utterance`` () =
    let input = ["i"]
    let expected = ["it"]
    let rule = "∅ → t / i_"
    testRule IPA rule input expected

[<Fact>]
let ``Insertion at the end after a set`` () =
    let input = ["i"]
    let expected = ["it"]
    let rule = """
    ∅ → t / V_
    V = (a, e, i, o, u)
    """
    testRule IPA rule input expected

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
    testRule IPA rule input expected

[<Fact>]
let ``Disjunct node`` () =
    let input = [ "la"; "ra" ]
    let expected = [ "lo"; "ro" ]
    let rule = "a → o / (l|r)_"
    testRule IPA rule input expected

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
    testRule IPA rule input expected

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
    testRule IPA rule input expected

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
    testRule X_SAMPA rule input expected

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
    testRule X_SAMPA rule input expected
