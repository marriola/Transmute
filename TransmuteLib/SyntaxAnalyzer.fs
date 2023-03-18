// Project:     TransmuteLib
// Module:      SyntaxAnalyzer
// Copyright:   (c) 2023 Matt Arriola
// License:     MIT

namespace TransmuteLib

module internal SyntaxAnalyzer =
    let rec private onlyEnvironmentMayContainPlaceholderNode kind (nodes: Node List) result =
        match nodes with
        | [] -> Ok result
        | Node.Untag (PlaceholderNode, position)::_ ->
            Result.Error (syntaxErrorMessage (sprintf "%s section cannot contain placeholder" kind) position)
        | _::xs ->
            onlyEnvironmentMayContainPlaceholderNode kind xs result

    let rec private onlyEnvironmentMayContainBoundaryNode kind (nodes: Node List) result =
        match nodes with
        | [] -> Ok result
        | Node.Untag (WordBoundaryNode, position)::_ ->
            Result.Error (syntaxErrorMessage (sprintf "%s section cannot contain boundary" kind) position)
        | _::xs ->
            onlyEnvironmentMayContainBoundaryNode kind xs result

    let rec private boundaryMayOnlyAppearAtEnds nodes result =
        match nodes with
        | [] ->
            Ok result
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
            boundaryMayOnlyAppearAtEnds rest result
        | Item (TaggedNode (position, WordBoundaryNode))::xsHead::_ ->
            match xsHead with
            // BoundaryNode at end is OK.
            | End ->
                Ok result
            // BoundaryNode in the middle is an error.
            | _ ->
                Result.Error (syntaxErrorMessage "Boundary may only appear at beginning or end of the environment section" position)
        | _::xs ->
            boundaryMayOnlyAppearAtEnds xs result

    let private isPlaceholder = function PlaceholderNode _ | TaggedNode (_, PlaceholderNode _) -> true | _ -> false

    let private onlyOnePlaceholderNodeIsAllowed nodes result =
        let rec validateInternal nodes found =
            match found, nodes with
            | _, [] -> Ok result
            | None, Node.Untag (PlaceholderNode _ as placeholder, _)::xs ->
                validateInternal xs (Some placeholder)
            | Some _, Node.Untag (PlaceholderNode _, position)::_ ->
                Result.Error (syntaxErrorMessage "Environment may only contain one placeholder" position)
            | _, _::xs ->
                validateInternal xs found
        validateInternal nodes None

    let private environmentNodeMustHavePlaceholderIfNotEmpty nodes result =
        if List.isEmpty nodes then
            Ok result
        elif nodes |> List.exists isPlaceholder then
            Ok result
        else
            match nodes with
            | TaggedNode (position, _)::_ ->
                Result.Error (syntaxErrorMessage "Environment must contain a placeholder if not empty" position)

    let private optionalNodeMayNotBeEmpty nodes result =
        let rec validateInternal (nodes: Node list) =
            match nodes with
            | [] -> Ok result
            | Node.Untag (OptionalNode [], position)::_ ->
                Result.Error (syntaxErrorMessage "Optional node may not be empty" position)
            | Node.Untag (DisjunctNode [], position)::_ ->
                Result.Error (syntaxErrorMessage "Disjunct node may not be empty" position)
            | _::rest ->
                validateInternal rest
        validateInternal nodes

    let private validateRuleNode input output environment nodes =                
        Ok nodes
        |> Result.bind (onlyEnvironmentMayContainBoundaryNode "Input" input)
        |> Result.bind (onlyEnvironmentMayContainBoundaryNode "Output" output)
        |> Result.bind (onlyEnvironmentMayContainPlaceholderNode "Input" input)
        |> Result.bind (onlyEnvironmentMayContainPlaceholderNode "Output" output)
        |> Result.bind (environmentNodeMustHavePlaceholderIfNotEmpty environment)
        |> Result.bind (onlyOnePlaceholderNodeIsAllowed environment)
        |> Result.bind (boundaryMayOnlyAppearAtEnds (BoundedList.fromList environment))
        |> Result.bind (optionalNodeMayNotBeEmpty environment)

    let validate nodes =
        let rec validateInternal rest out =
            out
            |> Result.bind (fun _ ->
                match rest with
                | [] -> out
                | TaggedNode (_, RuleNode (input, output, environment))::xs ->
                    out
                    |> Result.bind (validateRuleNode input output environment)
                    |> validateInternal xs
                | _::xs ->
                    validateInternal xs out)

        validateInternal nodes (Ok nodes)