module SyntaxAnalyzer
    open BoundedList
    open TransmuteLib
    open Exceptions

    let validateRuleNode target replacement environment =
        let rec onlyEnvironmentMayContainPlaceholderNode kind nodes =
            if nodes = [] then
                ()
            else
                let position, node = RuleParser.untagWithMetadata nodes.Head
                match node with
                | PlaceholderNode ->
                    raise (SyntaxException (sprintf "%s segment cannot contain placeholder" kind, position))
                | _ ->
                    onlyEnvironmentMayContainPlaceholderNode kind nodes.Tail
        let rec onlyEnvironmentMayContainBoundaryNode kind nodes =
            if nodes = [] then
                ()
            else
                let position, node = RuleParser.untagWithMetadata nodes.Head
                match node with
                | BoundaryNode ->
                    raise (SyntaxException (sprintf "%s segment cannot contain boundary" kind, position))
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
                    raise (SyntaxException ("Boundary may only appear at beginning or end of the environment segment", position))
            | _::xs ->
                boundaryMayOnlyAppearAtEnds xs
        let onlyOnePlaceholderNodeIsAllowed nodes =
            let rec validateInternal nodes found =
                if nodes = [] then
                    ()
                else
                    let position, node = RuleParser.untagWithMetadata nodes.Head
                    match node with
                    | PlaceholderNode ->
                        if found then
                            raise (SyntaxException ("Placeholder may not occur more than once", position))
                        else
                            validateInternal nodes.Tail true
                    | _ ->
                        validateInternal nodes.Tail found
            validateInternal nodes false
                
        onlyEnvironmentMayContainBoundaryNode "Target" target
        onlyEnvironmentMayContainBoundaryNode "Replacement" replacement
        onlyEnvironmentMayContainPlaceholderNode "Target" target
        onlyEnvironmentMayContainPlaceholderNode "Replacement" replacement
        onlyOnePlaceholderNodeIsAllowed environment
        boundaryMayOnlyAppearAtEnds (makeBoundedList environment)

    let validate (nodes: Node list) =
        let rec validateInternal (nodes: Node list) =
            if nodes = [] then
                OK
            else
                let position, head = RuleParser.untagWithMetadata nodes.Head
                match head with
                | RuleNode (target, replacement, environment) ->
                    validateRuleNode target replacement environment
                    validateInternal nodes.Tail
                | _ ->
                    validateInternal nodes.Tail
        try
            validateInternal nodes
        with
            :? SyntaxException as ex ->
                SyntaxError (ex.Message, ex.Position)
