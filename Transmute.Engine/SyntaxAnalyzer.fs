// Project:     Transmute.Engine
// Module:      SyntaxAnalyzer
// Copyright:   (c) 2026 Matt Arriola
// License:     MIT

namespace Transmute.Engine

open Transmute.Engine.ExceptionHelpers
open Position

module internal SyntaxAnalyzer =
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

    let rec private onlyEnvironmentMayContainPlaceholderNode kind (nodes: Node List) =
        match nodes with
        | [] -> Ok()
        | Node.Untag (PlaceholderNode, position)::_ ->
            Result.Error [syntaxErrorMessage (sprintf "%s section cannot contain placeholder" kind) position]
        | _::xs ->
            onlyEnvironmentMayContainPlaceholderNode kind xs

    let rec private onlyEnvironmentMayContainBoundaryNode kind (nodes: Node List) =
        match nodes with
        | [] -> Ok()
        | Node.Untag (WordBoundaryNode, position)::_ ->
            Result.Error [syntaxErrorMessage (sprintf "%s section cannot contain boundary" kind) position]
        | _::xs ->
            onlyEnvironmentMayContainBoundaryNode kind xs

    let rec private mayOnlyInsertUtterances inputNodes outputNodes =
        if inputNodes <> [] then
            Ok()
        else
            match outputNodes with
            | [] -> Ok()
            | Node.Untag (UtteranceNode _, _)::xs ->
                mayOnlyInsertUtterances inputNodes xs
            | Node.Untag (_, position)::_ ->
                Result.Error [syntaxErrorMessage "The output section can only contain utterances" position]

    let rec private boundaryMayOnlyAppearAtEnds nodes =
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
            boundaryMayOnlyAppearAtEnds rest
        | Item (TaggedNode (position, WordBoundaryNode))::xsHead::_ ->
            match xsHead with
            // BoundaryNode at end is OK.
            | End ->
                Ok()
            // BoundaryNode in the middle is an error.
            | _ ->
                Result.Error [syntaxErrorMessage "Boundary may only appear at beginning or end of the environment section" position]
        | _::xs ->
            boundaryMayOnlyAppearAtEnds xs

    let private isPlaceholder = function PlaceholderNode | TaggedNode (_, PlaceholderNode) -> true | _ -> false

    let private onlyOnePlaceholderNodeIsAllowed nodes =
        let rec validateInternal nodes found =
            match found, nodes with
            | _, [] -> Ok()
            | None, Node.Untag (PlaceholderNode as placeholder, _)::xs ->
                validateInternal xs (Some placeholder)
            | Some _, Node.Untag (PlaceholderNode, position)::_ ->
                Result.Error [syntaxErrorMessage "Environment may only contain one placeholder" position]
            | _, _::xs ->
                validateInternal xs found
        validateInternal nodes None

    let private environmentNodeMustHavePlaceholderIfNotEmpty nodes =
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
                    | None -> Result.Error [syntaxErrorMessage "Environment must contain a placeholder if not empty" position]
                | TaggedNode (position, _)::_ ->
                    Result.Error [syntaxErrorMessage "Environment must contain a placeholder if not empty" position]
        inner nodes

    let private optionalNodeMayNotBeEmpty nodes =
        let rec validateInternal (nodes: Node list) =
            match nodes with
            | [] -> Ok()
            | Node.Untag (OptionalNode [], position)::_ ->
                Result.Error [syntaxErrorMessage "Optional may not be empty" position]
            | Node.Untag (AlternationNode [], position)::_ ->
                Result.Error [syntaxErrorMessage "Alternation may not be empty" position]
            | _::rest ->
                validateInternal rest
        validateInternal nodes

    let private syllableSegmentsMustBeDefinedBeforeUse syllableRules nodes =
        let (hasOnset, hasNucleus, hasCoda) =
            syllableRules
            |> List.choose (function
                | SyllableDefinitionNode (_, onset, nucleus, coda) ->
                    Some (onset <> [], nucleus <> [], coda <> [])
                | _ ->
                    None)
            |> List.fold
                (fun (hasOnset, hasNucleus, hasCoda) (hasOnsetAlso, hasNucleusAlso, hasCodaAlso) ->
                    (hasOnset || hasOnsetAlso), (hasNucleus || hasNucleusAlso), (hasCoda || hasCodaAlso))
                (false, false, false)

        nodes
        |> walkTree (fun node position ->
            match node with
            | SyllableBoundaryNode SyllableEnd
            | SyllableBoundaryNode SyllableStart when not hasOnset && not hasNucleus && not hasCoda ->
                Result.Error [syntaxErrorMessage "Syllable definition must be provided" position]
            | SyllableBoundaryNode OnsetStart
            | SyllableBoundaryNode OnsetEnd when not hasOnset ->
                Result.Error [syntaxErrorMessage "Syllable onset definition must be provided" position]
            | SyllableBoundaryNode NucleusStart
            | SyllableBoundaryNode NucleusEnd when not hasNucleus ->
                Result.Error [syntaxErrorMessage "Syllable nucleus definition must be provided" position]
            | SyllableBoundaryNode CodaStart
            | SyllableBoundaryNode CodaEnd when not hasCoda ->
                Result.Error [syntaxErrorMessage "Syllable coda definition must be provided" position]
            | _ ->
                Ok())

    let private validateTransformationTargets nodes =
        nodes
        |> walkTree (fun node position ->
            match node with
            | SetIdentifierNode _ ->
                Result.Error [syntaxErrorMessage "Transformation target cannot be a set" position]
            | OptionalNode _ ->
                Result.Error [syntaxErrorMessage "Transformation target cannot be optional" position]
            | AlternationNode _ ->
                Result.Error [syntaxErrorMessage "Transformation target cannot be an alternation" position]
            | _ ->
                Ok())

    let internal validateIdentifiers features sets nodes =
        nodes
        |> walkTree (fun node position ->
            match node with
            | SetDefinitionNode (_, name, members)
            | FeatureDefinitionNode (_, name, members) ->
                let undefinedIdentifiers =
                    members
                    |> List.choose (function Node.Untag (SetIdentifierNode identifier, _) -> Some identifier | _ -> None)
                    |> List.filter (fun i -> not (Set.contains i features) && not (Set.contains i sets))
                    |> String.concat ", "

                if undefinedIdentifiers <> "" then
                    Error [syntaxErrorMessage $"Features and/or sets '{undefinedIdentifiers}' not defined" position]
                else
                    Ok()
                    
            | SetIdentifierNode identifier when not (Set.contains identifier features) && not (Set.contains identifier sets) ->
                Error [syntaxErrorMessage $"Feature or set '{identifier}' is not defined" position]
            | CompoundSetIdentifierNode setDesc ->
                setDesc
                |> walkTree (fun node position ->
                    match node with
                    | TermIdentifierNode identifier
                    | FeatureIdentifierNode (_, identifier) when not (Set.contains identifier features) && not (Set.contains identifier sets) ->
                        Error [syntaxErrorMessage $"Feature or set '{identifier}' is not defined" position]
                    | _ ->
                        Ok())
            | _ ->
                Ok())

    let private validateRuleNode features sets syllableRules node =
        let (RuleNode (_, _, input, output, environment)) = node

        let features =
            features
            |> Seq.map (function TaggedNode (_, FeatureDefinitionNode (_, name, _)) -> name)
            |> Set.ofSeq

        let sets =
            sets
            |> Seq.map (function TaggedNode (_, SetDefinitionNode (_, name, _)) -> name)
            |> Set.ofSeq

        let errors =
            [ onlyEnvironmentMayContainBoundaryNode "Input" input
              onlyEnvironmentMayContainBoundaryNode "Output" output
              onlyEnvironmentMayContainPlaceholderNode "Input" input
              onlyEnvironmentMayContainPlaceholderNode "Output" output
              mayOnlyInsertUtterances input output
              environmentNodeMustHavePlaceholderIfNotEmpty environment
              onlyOnePlaceholderNodeIsAllowed environment
              boundaryMayOnlyAppearAtEnds (BoundedList.fromList environment)
              optionalNodeMayNotBeEmpty environment
              syllableSegmentsMustBeDefinedBeforeUse syllableRules environment
              validateTransformationTargets output
              validateIdentifiers features sets input
              validateIdentifiers features sets output
              validateIdentifiers features sets environment ]
            |> List.choose (function Error message -> Some message | _ -> None)

        if errors.IsEmpty then
            Ok [node]
        else
            Error errors

    let internal validateCircularReferences features sets node =
        let nodeType =
            match Node.untag node with
            | SetDefinitionNode _ -> "set"
            | FeatureDefinitionNode _ -> "feature"

        let startPosition =
            match node with
            | TaggedNode (position, _) -> position

        let rec validateCircularReferences' visited node =
            let children =
                match node with
                | Node.Untag (SetDefinitionNode (_, _, children), _)
                | Node.Untag (FeatureDefinitionNode (_, _, children), _) ->
                    children

            let rec inner nodes =
                let lookup position name =
                    if List.exists (fst >> (=) name) visited then
                        let chain =
                            let list =
                                visited
                                |> Seq.map (fun (name, _) -> $"{name}")
                                |> Seq.rev
                                |> String.concat " → "
                            if name <> Node.getName node then
                                list + $" → {Node.getName node} → {name}"
                            else
                                list + $" → {Node.getName node}"

                        let lineNumbers =
                            let list =
                                visited
                                |> Seq.map (fun (_, p) -> p |> Position.getLine |> string)
                                |> Seq.rev
                                |> String.concat ", "
                            if name <> Node.getName node then
                                $", line numbers {list} and {Position.getLine position}"
                            else
                                ""

                        Error [syntaxErrorMessage $"Circular reference in {nodeType} definition: {chain}{lineNumbers}" startPosition]
                    else
                        Map.tryFind name features
                        |> Option.orElseWith (fun () -> Map.tryFind name sets)
                        // Ignore undefined sets at this stage
                        |> Option.defaultValue (SetDefinitionNode (-1, "", []))
                        |> validateCircularReferences' ((Node.getName node, position) :: visited)

                match nodes with
                | [] ->
                    Ok [node]
                | Node.Untag (SetIdentifierNode identifier, position) :: rest ->
                    let result = lookup position identifier
                    if Result.isError result then
                        result
                    else
                        inner rest
                | _ :: rest ->
                    inner rest

            inner children

        validateCircularReferences' [] node

    let private validateSetDefinitionNode features sets node =
        let features = Node.getFeatureMap features
        let sets = Node.getSetMap sets

        let errors =
            [ validateCircularReferences features sets node ]
            |> List.choose (function Error message -> Some message | _ -> None)

        if errors.IsEmpty then
            Ok [node]
        else
            Error errors

    let accumulate rules errors result =
        match result with
        | Ok rule ->
            (rule @ rules), errors
        | Error messages ->
            rules, (messages @ errors)

    /// <summary>
    /// Validates a node list result.
    /// </summary>
    /// <param name="nodes">A result object of the list of rule nodes to validate.</param>
    /// <returns>A tuple of a list of validated rule nodes and a list of error messages. If the incoming result value was error, no rules and that error are returned.</returns>
    let validate parseResult : Node list * string list =
        match parseResult with
        | Error message ->
            [], [message]

        | Ok nodes ->
            let features = nodes |> List.choose (function TaggedNode (_, (FeatureDefinitionNode _)) as node -> Some node | _ -> None)
            let sets = nodes |> List.choose (function TaggedNode (_, (SetDefinitionNode _)) as node -> Some node | _ -> None)
            let syllableRules = nodes |> List.collect (function SyllableDefinitionListNode (_, defs) -> defs | _ -> [])

            let rec validateInternal rest (rulesOut, errorsOut) =
                match rest with
                | [] ->
                    List.rev rulesOut, errorsOut |> List.rev |> List.collect id

                | SyllableDefinitionListNode _ as node :: rest ->
                    validateInternal rest ((node :: rulesOut), errorsOut)

                | TaggedNode (_, (RuleNode _ as ruleNode))::rest ->
                    validateRuleNode features sets syllableRules ruleNode
                    |> accumulate rulesOut errorsOut
                    |> validateInternal rest

                | (TaggedNode (_, SetDefinitionNode _) as setNode) :: rest
                | (TaggedNode (_, FeatureDefinitionNode _) as setNode) :: rest ->
                    validateSetDefinitionNode features sets setNode
                    |> accumulate rulesOut errorsOut
                    |> validateInternal rest

                | _::xs ->
                    validateInternal xs (rulesOut, errorsOut)

            validateInternal nodes ([], [])
