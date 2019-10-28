namespace TransmuteLib

module SyntaxAnalyzer =
    open TransmuteLib.Exceptions

    let private validateRuleNode target replacement environment =
        let rec onlyEnvironmentMayContainPlaceholderNode kind (nodes: Node List) =
            if nodes.IsEmpty then
                ()
            else
                let position, node = Node.untagWithMetadata nodes.Head
                match node with
                | PlaceholderNode ->
                    invalidSyntax (sprintf "%s segment cannot contain placeholder" kind) position
                | _ ->
                    onlyEnvironmentMayContainPlaceholderNode kind nodes.Tail

        let rec onlyEnvironmentMayContainBoundaryNode kind (nodes: Node List) =
            if nodes.IsEmpty then
                ()
            else
                let position, node = Node.untagWithMetadata nodes.Head
                match node with
                | BoundaryNode ->
                    invalidSyntax (sprintf "%s segment cannot contain boundary" kind) position
                | _ ->
                    onlyEnvironmentMayContainBoundaryNode kind nodes.Tail

        let rec boundaryMayOnlyAppearAtEnds nodes =
            match nodes with
            | [] ->
                ()
            | Begin::xs ->
                let rest =
                    match xs.Head with
                    // BoundaryNode at beginning is OK.
                    // Skip over it because we already know about it.
                    | Item (TaggedNode (_, BoundaryNode)) ->
                        xs.Tail
                    // Everything else is OK.
                    | _ ->
                        xs
                boundaryMayOnlyAppearAtEnds rest
            | Item (TaggedNode (position, BoundaryNode))::xs ->
                match xs.Head with
                // BoundaryNode at end is OK.
                | End ->
                    ()
                // BoundaryNode in the middle is an error.
                | _ ->
                    invalidSyntax "Boundary may only appear at beginning or end of the environment segment" position
            | _::xs ->
                boundaryMayOnlyAppearAtEnds xs

        let onlyOnePlaceholderNodeIsAllowed nodes =
            let rec validateInternal (nodes: Node List) found =
                if nodes.IsEmpty then
                    ()
                else
                    let position, node = Node.untagWithMetadata nodes.Head
                    match node with
                    | PlaceholderNode ->
                        if found then
                            invalidSyntax "Placeholder may not occur more than once" position
                        else
                            validateInternal nodes.Tail true
                    | _ ->
                        validateInternal nodes.Tail found
            validateInternal nodes false

        let optionalNodeMayNotBeEmpty nodes =
            let rec validateInternal (nodes: Node list) =
                if nodes.IsEmpty then
                    ()
                else
                    let position, node = Node.untagWithMetadata nodes.Head
                    match node with
                    | OptionalNode [] ->
                        invalidSyntax "Optional node may not be empty" position
                    | _ ->
                        validateInternal nodes.Tail
            validateInternal nodes
                
        onlyEnvironmentMayContainBoundaryNode "Target" target
        onlyEnvironmentMayContainBoundaryNode "Replacement" replacement
        onlyEnvironmentMayContainPlaceholderNode "Target" target
        onlyEnvironmentMayContainPlaceholderNode "Replacement" replacement
        onlyOnePlaceholderNodeIsAllowed environment
        boundaryMayOnlyAppearAtEnds (BoundedList.fromList environment)
        optionalNodeMayNotBeEmpty environment

    let validate (nodes: Node list) =
        let rec validateInternal (nodes: Node list) =
            if nodes.IsEmpty then
                OK
            else
                match nodes with
                | TaggedNode (_, RuleNode (target, replacement, environment))::_ ->
                    validateRuleNode target replacement environment
                    validateInternal nodes.Tail
                | _ ->
                    validateInternal nodes.Tail
        try
            validateInternal nodes
        with
            Exceptions.SyntaxError (message, row, col) ->
                ValidateResult.SyntaxError (message, (row, col))
