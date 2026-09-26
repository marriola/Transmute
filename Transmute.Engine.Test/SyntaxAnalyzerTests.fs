// Project:     Transmute.Engine.Test
// Module:      Syntax analyzer tests
// Copyright:   (c) 2026 Matt Arriola
// License:     MIT

module Transmute.Engine.Test.SyntaxAnalyzerTests

open Transmute.Engine
open Transmute.Engine.Position
open Xunit
open Transmute.Engine.Utils.Operators

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
    let features = Node.getFeatureMap nodes
    let sets = Node.getSetMap nodes

    let actual = SyntaxAnalyzer.validateCircularReferences features sets nodes[0]
    let expected = Error ["Syntax error at line 2, column 1 (offset 2): Circular reference in set definition: A → B → C → A, line numbers 2, 3 and 4"]
    
    Assert.Equal<string list>(resultMessage expected, resultMessage actual)
