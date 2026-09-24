// Project:     Transmute.Engine
// Module:      SyntaxAnalyzer
// Copyright:   (c) 2026 Matt Arriola
// License:     MIT

namespace Transmute.Engine

open Transmute.Engine.ExceptionHelpers

module private SyntaxAnalyzer =
    let private walkTree fVisit nodes =
        let rec inner nodes =
            match nodes with
            | Node.Untag (node, position) :: rest ->
                fVisit node position
            | _ ->
                Ok()
            |> Result.bind (fun _ ->
                match nodes with
                | [] ->
                    Ok()
                | Node.Untag (OptionalNode optionalNodes, _) :: rest ->
                    inner optionalNodes
                    |> Result.bind (fun _ -> inner rest)
                | Node.Untag (AlternationNode alternations, _) :: rest ->
                    inner (List.concat alternations)
                    |> Result.bind (fun _ -> inner rest)
                | Node.Untag (NegationNode negation, _) :: rest ->
                    inner [ negation ]
                    |> Result.bind (fun _ -> inner rest)
                | _ :: rest ->
                    inner rest)

        inner nodes

    let rec private onlyEnvironmentMayContainPlaceholderNode kind (nodes: Node List) () =
        match nodes with
        | [] -> Ok()
        | Node.Untag (PlaceholderNode, position)::_ ->
            Result.Error (syntaxErrorMessage (sprintf "%s section cannot contain placeholder" kind) position)
        | _::xs ->
            onlyEnvironmentMayContainPlaceholderNode kind xs ()

    let rec private onlyEnvironmentMayContainBoundaryNode kind (nodes: Node List) () =
        match nodes with
        | [] -> Ok()
        | Node.Untag (WordBoundaryNode, position)::_ ->
            Result.Error (syntaxErrorMessage (sprintf "%s section cannot contain boundary" kind) position)
        | _::xs ->
            onlyEnvironmentMayContainBoundaryNode kind xs ()

    let rec private mayOnlyInsertUtterances inputNodes outputNodes () =
        if inputNodes <> [] then
            Ok()
        else
            match outputNodes with
            | [] -> Ok()
            | Node.Untag (UtteranceNode _, _)::xs ->
                mayOnlyInsertUtterances inputNodes xs ()
            | Node.Untag (_, position)::_ ->
                Result.Error (syntaxErrorMessage "The output section can only contain utterances" position)

    let rec private boundaryMayOnlyAppearAtEnds nodes () =
        match nodes with
        | [] ->
            Ok()
        | Begin::(xsHead::xsRest as xs) ->
            let rest =
                match xsHead with
                // BoundaryNode at beginning is OK.
                // Skip over it because we already know about it.
                | Item (TaggedNode (_, WordBoundaryNode)) ->
                    xsRest
                // Everything else is OK.
                | _ ->
                    xs
            boundaryMayOnlyAppearAtEnds rest ()
        | Item (TaggedNode (position, WordBoundaryNode))::xsHead::_ ->
            match xsHead with
            // BoundaryNode at end is OK.
            | End ->
                Ok()
            // BoundaryNode in the middle is an error.
            | _ ->
                Result.Error (syntaxErrorMessage "Boundary may only appear at beginning or end of the environment section" position)
        | _::xs ->
            boundaryMayOnlyAppearAtEnds xs ()

    let private isPlaceholder = function PlaceholderNode | TaggedNode (_, PlaceholderNode) -> true | _ -> false

    let private onlyOnePlaceholderNodeIsAllowed nodes () =
        let rec validateInternal nodes found =
            match found, nodes with
            | _, [] -> Ok()
            | None, Node.Untag (PlaceholderNode as placeholder, _)::xs ->
                validateInternal xs (Some placeholder)
            | Some _, Node.Untag (PlaceholderNode, position)::_ ->
                Result.Error (syntaxErrorMessage "Environment may only contain one placeholder" position)
            | _, _::xs ->
                validateInternal xs found
        validateInternal nodes None

    let private environmentNodeMustHavePlaceholderIfNotEmpty nodes () =
        let rec inner nodes =
            if List.isEmpty nodes then
                Ok()
            elif nodes |> List.exists isPlaceholder then
                Ok()
            else
                match nodes with
                | TaggedNode (_, OptionalNode children)::[] ->
                    inner children
                | TaggedNode (position, AlternationNode branches)::[] ->
                    match List.tryFind (inner >> function Ok _ -> true | _ -> false) branches with
                    | Some _ -> Ok()
                    | None -> Result.Error (syntaxErrorMessage "Environment must contain a placeholder if not empty" position)
                | TaggedNode (position, _)::_ ->
                    Result.Error (syntaxErrorMessage "Environment must contain a placeholder if not empty" position)
        inner nodes

    let private optionalNodeMayNotBeEmpty nodes () =
        let rec validateInternal (nodes: Node list) =
            match nodes with
            | [] -> Ok()
            | Node.Untag (OptionalNode [], position)::_ ->
                Result.Error (syntaxErrorMessage "Optional may not be empty" position)
            | Node.Untag (AlternationNode [], position)::_ ->
                Result.Error (syntaxErrorMessage "Alternation may not be empty" position)
            | _::rest ->
                validateInternal rest
        validateInternal nodes

    let private syllableSegmentsMustBeDefinedBeforeUse allNodes nodes () =
        let (hasOnset, hasNucleus, hasCoda) =
            allNodes
            |> List.collect (fun node ->
                match node with
                | Node.Untag (SyllableDefinitionListNode (_, definitions), _) ->
                    definitions
                    |> List.choose (function
                        SyllableDefinitionNode (_, onset, nucleus, coda) ->
                            Some (onset <> [], nucleus <> [], coda <> [])
                        | _ ->
                            None)
                | _ ->
                    [])
            |> List.fold
                (fun (hasOnset, hasNucleus, hasCoda) (hasOnsetAlso, hasNucleusAlso, hasCodaAlso) ->
                    (hasOnset || hasOnsetAlso), (hasNucleus || hasNucleusAlso), (hasCoda || hasCodaAlso))
                (false, false, false)

        nodes
        |> walkTree (fun node position ->
            match node with
            | SyllableBoundaryNode SyllableEnd
            | SyllableBoundaryNode SyllableStart when not hasOnset && not hasNucleus && not hasCoda ->
                Result.Error (syntaxErrorMessage "Syllable definition must be provided" position)
            | SyllableBoundaryNode OnsetStart
            | SyllableBoundaryNode OnsetEnd when not hasOnset ->
                Result.Error (syntaxErrorMessage "Syllable onset definition must be provided" position)
            | SyllableBoundaryNode NucleusStart
            | SyllableBoundaryNode NucleusEnd when not hasNucleus ->
                Result.Error (syntaxErrorMessage "Syllable nucleus definition must be provided" position)
            | SyllableBoundaryNode CodaStart
            | SyllableBoundaryNode CodaEnd when not hasCoda ->
                Result.Error (syntaxErrorMessage "Syllable coda definition must be provided" position)
            | _ ->
                Ok())

    let private validateTransformationTargets nodes () =
        nodes
        |> walkTree (fun node position ->
            match node with
            | SetIdentifierNode _ ->
                Result.Error (syntaxErrorMessage $"Transformation target cannot be a set" position)
            | OptionalNode _ ->
                Result.Error (syntaxErrorMessage "Transformation target cannot be optional" position)
            | AlternationNode _ ->
                Result.Error (syntaxErrorMessage "Transformation target cannot be an alternation" position)
            | _ ->
                Ok())

    let private validateIdentifiers features sets nodes () =
        nodes
        |> walkTree (fun node position ->
            match node with
            | SetIdentifierNode identifier when not (Set.contains identifier features) && not (Set.contains identifier sets) ->
                Error (syntaxErrorMessage $"Feature or set '{identifier}' is not defined" position)
            | CompoundSetIdentifierNode setDesc ->
                setDesc
                |> walkTree (fun node position ->
                    match node with
                    | TermIdentifierNode identifier
                    | FeatureIdentifierNode (_, identifier) when not (Set.contains identifier features) && not (Set.contains identifier sets) ->
                        Error (syntaxErrorMessage $"Feature or set '{identifier}' is not defined" position)
                    | _ ->
                        Ok())
            | _ ->
                Ok())

    let private validateRuleNode features sets input output environment nodes =
        Ok()
        |> Result.bind (onlyEnvironmentMayContainBoundaryNode "Input" input)
        |> Result.bind (onlyEnvironmentMayContainBoundaryNode "Output" output)
        |> Result.bind (onlyEnvironmentMayContainPlaceholderNode "Input" input)
        |> Result.bind (onlyEnvironmentMayContainPlaceholderNode "Output" output)
        |> Result.bind (mayOnlyInsertUtterances input output)
        |> Result.bind (environmentNodeMustHavePlaceholderIfNotEmpty environment)
        |> Result.bind (onlyOnePlaceholderNodeIsAllowed environment)
        |> Result.bind (boundaryMayOnlyAppearAtEnds (BoundedList.fromList environment))
        |> Result.bind (optionalNodeMayNotBeEmpty environment)
        |> Result.bind (syllableSegmentsMustBeDefinedBeforeUse nodes environment)
        |> Result.bind (validateTransformationTargets output)
        |> Result.bind (validateIdentifiers features sets input)
        |> Result.bind (validateIdentifiers features sets output)
        |> Result.bind (validateIdentifiers features sets environment)
        |> Result.map (fun _ -> nodes)

    let validate nodes =
        let features =
            nodes
            |> Seq.choose (function TaggedNode (_, FeatureDefinitionNode (_, name, _)) as feature -> Some name | _ -> None)
            |> Set.ofSeq

        let sets =
            nodes
            |> Seq.choose (function TaggedNode (_, SetDefinitionNode (_, name, _)) as set -> Some name | _ -> None)
            |> Set.ofSeq

        let rec validateInternal rest out =
            out
            |> Result.bind (fun _ ->
                match rest with
                | [] -> out
                | TaggedNode (_, RuleNode (_, _, input, output, environment))::xs ->
                    out
                    |> Result.bind (validateRuleNode features sets input output environment)
                    |> validateInternal xs
                | _::xs ->
                    validateInternal xs out)

        validateInternal nodes (Ok nodes)