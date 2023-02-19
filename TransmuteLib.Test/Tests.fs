module Tests

open TransmuteLib
open Xunit

let testRule rules inputs expected =
    let actual =
        rules
        |> RuleParser.parseRules
        |> Result.map (fun (features, sets, rules) -> RuleCompiler.compile false features sets rules[0])
        |> Result.map (fun rule -> inputs |> List.map (RuleMachine.transform false rule))
    Assert.Equal(Ok expected, actual)

let testRules rules inputs expected =
    let rules =
        rules
        |> RuleParser.parseRules
        |> Result.map (fun (features, sets, rules) -> RuleCompiler.compileRulesParallel false features sets rules)
    let transform input =
        rules
        |> Result.map (fun rules ->
            (input, rules)
            ||> List.fold (fun input rule -> RuleMachine.transform false rule input))
    let actual = inputs |> List.map transform
    Assert.Equal<Result<string, string>>(List.map Result<string,string>.Ok expected, actual)

[<Fact>]
let ``Unconditional single phone transformation`` () =
    testRule "k/t/_" ["ka"] ["ta"]

[<Fact>]
let ``Unconditional set transformation`` () =
    let input = [ "ka"; "pa"; "ta" ]
    let expected = [ "ga"; "ba"; "da" ]
    let rule =
        """
        [$voiced] {
            k -> g
            p -> b
            t -> d
        }

        [-$voiced]/[+$voiced]/_
        """
    testRule rule input expected

[<Fact>]
let ``Unconditional set reverse transformation`` () =
    let input = [ "ga"; "ba"; "da" ]
    let expected = [ "ka"; "pa"; "ta" ]
    let rule =
        """
        [$voiced] {
            k -> g
            p -> b
            t -> d
        }

        [+$voiced]/[-$voiced]/_
        """
    testRule rule input expected

[<Fact>]
let ``Deletion`` () =
    let input = ["ha"]
    let expected = ["a"]
    let rule = "h//_"
    testRule rule input expected

[<Fact>]
let ``Insertion`` () =
    let input = ["ot"]
    let expected = ["oit"]
    let rule = "∅/i/o_t"
    testRule rule input expected

[<Fact>]
let ``Addition`` () =
    let input = ["gʰˈlχʷtom"]
    let expected = ["gʰˈulχʷtom"]
    let rule =
        """
        $DENTAL { t d dʰ θ ð s z }
        $LABIAL { m p b bʰ ɸ β }
        $LABIOVELAR { kʷ gʷ gʷʰ xʷ ɣʷ }
        $VELAR { k g gʰ x ɣ $LABIOVELAR }
        $SONORANT { m n l r w j }
        $LIQUID { l r }
        $GLIDE { w j }
        $NASAL { m n }
        $LARYNGEAL { ʔ χ χʷ }
        $SIBILANT { s }
        $C { $STOP $DENTAL $LABIAL $VELAR $SONORANT $LIQUID $GLIDE $NASAL $LARYNGEAL $SIBILANT }
        $LARYNGEAL { ʔ χ χʷ }
        ∅→u/(#|[$C-$LARYNGEAL])(ˈ)_(m|n|l|r)(#|$C)
        """
    testRule rule input expected

[<Fact>]
let ``Disjunct node`` () =
    let input = [ "la"; "ra" ]
    let expected = [ "lo"; "ro" ]
    let rule = "a/o/(l|r)_"
    testRule rule input expected

[<Fact>]
let ``Compound sets`` () =
    let input = ["ka"]
    let expected = ["a"]
    let rules =
        """
        $STOP { k p t g b d }
        [$fricative] {
            k -> x
            p -> ɸ
            t -> θ
            g -> ɣ
            b -> β
            d -> ð
        }
        [$voiced] {
            k -> g
            p -> b
            t -> d
            x -> ɣ
            ɸ -> β
            θ -> ð
        }

        [$STOP-$voiced]/[+$voiced]/_                ; ka -> ga
        [+$voiced-$fricative]/[+$fricative]/_       ; ga -> ɣa
        ɣ//_                                        ; ɣa -> a
        """
    testRules rules input expected

[<Fact>]
let ``Multi-phoneme set transformations`` () =
    let input = [ "ta"; "twa"; "sa"; "da"; "dwa" ]
    let expected = [ "ta"; "ta"; "tsa"; "da"; "da" ]
    let rules =
        """
        [$labialized] {
            t -> tw
            d -> dw
        }
        [$affricate] {
            s -> ts
        }

        [+$labialized]/[-$labialized]/_
        [-$affricate]/[+$affricate]/_
        """
    testRules rules input expected

[<Fact>]
let ``Repeat simple replacement`` () =
    let input = ["kak"]
    let expected = ["xax"]
    let rule = "k/x/_"
    testRule rule input expected

[<Fact>]
let ``Repeat set replacement`` () =
    let input = ["gʷakʷ"]
    let expected = ["ɣʷaxʷ"]
    let rule =
        """
        [$fricative] {
            k -> x
            kʷ -> xʷ
            g -> ɣ
            gʷ -> ɣʷ
        }

        [-$fricative]/[+$fricative]/_
        """
    testRule rule input expected