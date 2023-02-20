namespace TransmuteLib

module internal SyntaxAnalyzer =
    let rec private onlyEnvironmentMayContainPlaceholderNode kind (nodes: Node List) result =
        match nodes with
        | [] -> Ok nodes
        | Node.Untag (PlaceholderNode, position)::_ ->
            Result.Error (syntaxErrorMessage (sprintf "%s segment cannot contain placeholder" kind) position)
        | _::xs ->
            onlyEnvironmentMayContainPlaceholderNode kind xs result

    let rec private onlyEnvironmentMayContainBoundaryNode kind (nodes: Node List) result =
        match nodes with
        | [] -> Ok nodes
        | Node.Untag (BoundaryNode, position)::_ ->
            Result.Error (syntaxErrorMessage (sprintf "%s segment cannot contain boundary" kind) position)
        | _::xs ->
            onlyEnvironmentMayContainBoundaryNode kind xs result

    let rec private boundaryMayOnlyAppearAtEnds nodes result =
        match nodes with
        | [] ->
            Ok nodes
        | Begin::(xsHead::xsRest as xs) ->
            let rest =
                match xsHead with
                // BoundaryNode at beginning is OK.
                // Skip over it because we already know about it.
                | Item (TaggedNode (_, BoundaryNode)) ->
                    xsRest
                // Everything else is OK.
                | _ ->
                    xs
            boundaryMayOnlyAppearAtEnds rest result
        | Item (TaggedNode (position, BoundaryNode))::xsHead::_ ->
            match xsHead with
            // BoundaryNode at end is OK.
            | End ->
                Ok nodes
            // BoundaryNode in the middle is an error.
            | _ ->
                Result.Error (syntaxErrorMessage "Boundary may only appear at beginning or end of the environment segment" position)
        | _::xs ->
            boundaryMayOnlyAppearAtEnds xs result

    let private onlyOnePlaceholderNodeIsAllowed nodes result =
        let rec validateInternal (nodes: Node List) found =
            match nodes with
            | [] -> Ok nodes
            | Node.Untag (PlaceholderNode, position)::_ ->
                if found then
                    Result.Error (syntaxErrorMessage "Placeholder may not occur more than once" position)
                else
                    validateInternal nodes.Tail true
            | _::xs ->
                validateInternal xs found
        validateInternal nodes false

    let private optionalNodeMayNotBeEmpty nodes result =
        let rec validateInternal (nodes: Node list) =
            match nodes with
            | [] -> Ok nodes
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
        |> Result.bind (onlyOnePlaceholderNodeIsAllowed environment)
        |> Result.bind (boundaryMayOnlyAppearAtEnds (BoundedList.fromList environment))
        |> Result.bind (optionalNodeMayNotBeEmpty environment)

    let validate nodes =
        let rec validateInternal out rest =
            out
            |> Result.bind (fun _ ->
                match rest with
                | [] -> out
                | TaggedNode (_, RuleNode (input, output, environment))::xs ->
                    Result.bind
                        (validateInternal out)
                        (validateRuleNode input output environment xs)
                | _::xs ->
                    validateInternal out xs)

        validateInternal (Ok nodes) nodes