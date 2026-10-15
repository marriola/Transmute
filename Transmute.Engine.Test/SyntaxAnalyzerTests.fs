// Project:     Transmute.Engine.Test
// Module:      Syntax analyzer tests
// Copyright:   (c) 2026 Matt Arriola
// License:     MIT

module Transmute.Engine.Test.SyntaxAnalyzerTests

open Transmute.Engine
open Transmute.Engine.Position
open Transmute.Engine.Utils.Operators
open Xunit

let resultMessage = function
    | Ok _ -> []
    | Error messages -> messages

let parse content =
    let tokens =
        match Lexer.lex IPA content with
        | Error (msg, Offset offset, Line row, Column col) ->
            Result.Error (sprintf "Syntax error at row %d column %d (offset %d): %s" row col offset msg)
        | Ok tokens ->
            Result.Ok tokens

    tokens
    |> Result.bind (RuleParser.parseInternal (new System.Collections.Generic.List<Token>()))
    |> Result.orAbort
            
[<Fact>]
let ``Circular references are detected`` () =
    let rule =
        """
A = (B)
B = (C)
C = (A)
"""

    let nodes = parse rule
    let features = nodes |> Node.getFeatureMap
    let sets = nodes |> Node.getSetMap
    let setNode = nodes[0]

    let actual =
        SyntaxAnalyzer.validateCircularReferences features sets setNode
        |> resultMessage

    let expected = "Syntax error at line 2, column 1 (offset 2): Circular reference in set definition: A → B → C → A, line numbers 2, 3 and 4"
    
    Assert.Equal<string list>([expected], actual)

[<Fact>]
let ``Nonexistent set references are caught in syllable rules`` () =
    let rule =
        """
Syllable = (
    Onset = b
    Nucleus = a
    Coda = Jimbob
)

a -> b
"""

    let nodes = parse rule
    let features = nodes |> Node.getFeatureMap |> Map.values
    let sets = nodes |> Node.getFeatureMap |> Map.values
    let syllableNode = nodes[0]

    let actual =
        SyntaxAnalyzer.validateSyllableRule features sets syllableNode
        |> resultMessage

    let expected = "Syntax error at line 5, column 12 (offset 59): Feature or set 'Jimbob' is not defined"

    Assert.Equal<string list>([expected],  actual)
