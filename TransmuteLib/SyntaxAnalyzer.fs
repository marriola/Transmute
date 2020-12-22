namespace TransmuteLib

module SyntaxAnalyzer =
    let inline (?|>) x f =
        match x with
        | Ok _ -> f ()
        | Result.Error _ -> x

    let rec private onlyEnvironmentMayContainPlaceholderNode kind (nodes: Node List) () =
        match nodes with
        | [] -> Ok ()
        | Node.Untag (PlaceholderNode, position)::_ ->
            Result.Error (syntaxErrorMessage (sprintf "%s segment cannot contain placeholder" kind) position)
        | _::xs ->
            onlyEnvironmentMayContainPlaceholderNode kind xs ()

    let rec private onlyEnvironmentMayContainBoundaryNode kind (nodes: Node List) () =
        match nodes with
        | [] -> Ok ()
        | Node.Untag (BoundaryNode, position)::_ ->
            Result.Error (syntaxErrorMessage (sprintf "%s segment cannot contain boundary" kind) position)
        | _::xs ->
            onlyEnvironmentMayContainBoundaryNode kind xs ()

    let rec private boundaryMayOnlyAppearAtEnds nodes () =
        match nodes with
        | [] ->
            Ok ()
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
            boundaryMayOnlyAppearAtEnds rest ()
        | Item (TaggedNode (position, BoundaryNode))::xsHead::_ ->
            match xsHead with
            // BoundaryNode at end is OK.
            | End ->
                Ok ()
            // BoundaryNode in the middle is an error.
            | _ ->
                Result.Error (syntaxErrorMessage "Boundary may only appear at beginning or end of the environment segment" position)
        | _::xs ->
            boundaryMayOnlyAppearAtEnds xs ()

    let private onlyOnePlaceholderNodeIsAllowed nodes () =
        let rec validateInternal (nodes: Node List) found =
            match nodes with
            | [] -> Ok ()
            | Node.Untag (PlaceholderNode, position)::_ ->
                if found then
                    Result.Error (syntaxErrorMessage "Placeholder may not occur more than once" position)
                else
                    validateInternal nodes.Tail true
            | _::xs ->
                validateInternal xs found
        validateInternal nodes false

    let private optionalNodeMayNotBeEmpty nodes () =
        let rec validateInternal (nodes: Node list) =
            match nodes with
            | [] -> Ok ()
            | Node.Untag (OptionalNode [], position)::_ ->
                Result.Error (syntaxErrorMessage "Optional node may not be empty" position)
            | Node.Untag (DisjunctNode [], position)::_ ->
                Result.Error (syntaxErrorMessage "Disjunct node may not be empty" position)
            | _::rest ->
                validateInternal rest
        validateInternal nodes

    let private validateRuleNode target replacement environment =                
        Ok ()
        ?|> onlyEnvironmentMayContainBoundaryNode "Target" target
        ?|> onlyEnvironmentMayContainBoundaryNode "Replacement" replacement
        ?|> onlyEnvironmentMayContainPlaceholderNode "Target" target
        ?|> onlyEnvironmentMayContainPlaceholderNode "Replacement" replacement
        ?|> onlyOnePlaceholderNodeIsAllowed environment
        ?|> boundaryMayOnlyAppearAtEnds (BoundedList.fromList environment)
        ?|> optionalNodeMayNotBeEmpty environment

    let validate nodes =
        let rec validateInternal nodes =
            match nodes with
            | [] -> Ok ()
            | TaggedNode (_, RuleNode (target, replacement, environment))::xs ->
                validateRuleNode target replacement environment
                validateInternal xs
            | _::xs ->
                validateInternal xs

        validateInternal nodes