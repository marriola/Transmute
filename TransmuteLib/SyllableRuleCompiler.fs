// Project:     Transmute
// Module:      SyllableRuleCompiler
// Description: Combinator-based syllable rule compiler
// Copyright:   (c) 2026 Matt Arriola
// License:     MIT

namespace TransmuteLib

open TransmuteLib.RuleCombinators

module internal SyllableRuleCompiler =

    /// Converts feature and set maps to lists, sorted in descending order of input length.
    let private sortFeaturesAndSets (features: Map<string, Node>) (sets: Map<string, Node>) =
        let alphabet = Node.getAlphabet features sets

        let features =
            features
            |> Map.map (fun _ feature ->
                match feature with
                | FeatureDefinitionNode (_, members) ->
                    members
                    |> List.map (fun item ->
                        match item with
                        | TransformationNode (UtteranceNode input, UtteranceNode output) ->
                            input, output
                        | UtteranceNode utterance ->
                            utterance, utterance)
                    |> List.sortByDescending (fst >> String.length))

        let sets =
            sets
            |> Map.map (fun _ set ->
                match set with
                | SetDefinitionNode (_, members) ->
                    members
                    |> List.map (fun (UtteranceNode utterance) -> utterance)
                    |> List.sortByDescending String.length)

        alphabet, features, sets

    let compile features sets (ruleNode: Node) =

        let alphabet, featureList, setList = sortFeaturesAndSets features sets
        
        let build label nodes (rule: (State -> State)) =
            let rec inner i level nodes (rule: (State -> State)) =
                match nodes with
                | [] ->
                    rule

                | OptionalNode optionalNodes :: rest ->
                    let optionalRule = inner 0 (level + 1) optionalNodes State.id

                    rule
                    >> log i level "begin optional"
                    >> matchOptional optionalRule
                    >> log i level "end optional"
                    |> inner (i + 1) level rest

                | AlternationNode alternationNodes :: rest ->
                    let alternationRules =
                        alternationNodes
                        |> List.map (fun nodes -> inner 0 (level + 1) nodes State.id)

                    rule
                    >> log i level "begin alternation"
                    >> matchOneOf alternationRules
                    >> log i level "end alternation"
                    |> inner (i + 1) level rest

                | WordBoundaryNode :: rest ->
                    rule
                    >> matchWordBoundary
                    >> log i level "word boundary"
                    >> thenReplaceWith label
                    |> inner (i + 1) level rest

                | SyllableBoundaryNode boundaryType :: rest ->
                    rule
                    >> matchSyllableBoundary boundaryType
                    >> log i level "syllable boundary"
                    |> inner (i + 1) level rest

                | UtteranceNode utterance :: rest ->
                    rule
                    >> matchSymbols utterance
                    >> thenReplaceWithCopies label
                    >> log i level $"/{utterance}/"
                    |> inner (i + 1) level rest

                | SetIdentifierNode name :: rest ->
                    let setRules = List.map matchSymbols setList[name]

                    rule
                    >> matchOneOf setRules
                    >> thenReplaceWithCopies label
                    >> log i level $"set {name}"
                    |> inner (i + 1) level rest

                | CompoundSetIdentifierNode setDescriptor :: rest ->
                    let phonemeRules =
                        setDescriptor
                        |> Node.setIntersection alphabet features sets
                        |> List.sortByDescending String.length
                        |> List.map matchSymbols

                    rule
                    >> matchOneOf phonemeRules
                    >> thenReplaceWithCopies label
                    >> log i level (setDescriptor.ToString())
                    |> inner (i + 1) level rest

            inner 0 0 nodes rule

        match ruleNode with
        | SyllableDefinitionNode (onset, nucleus, coda) ->
            let rule =
                (beginRule >> thenReplaceWith ".")
                |> build "O" (Node.untagAll onset)
                |> build "N" (Node.untagAll nucleus)
                |> build "C" (Node.untagAll coda)
                >> endRule

            State.wrap >> repeat rule >> State.unwrapRest
