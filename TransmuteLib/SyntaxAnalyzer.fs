module SyntaxAnalyzer
    open BoundedList
    open TransmuteLib
    open Exceptions

    let validateRuleNode target replacement environment =
        let rec onlyLastSegmentMayContainBoundaryNode kind nodes =
            if nodes = [] then
                ()
            else
                let position, node = RuleParser.untagWithMetadata nodes.Head
                match node with
                | BoundaryNode ->
                    raise (SyntaxException (sprintf "%s segment cannot contain boundary" kind, position))
                | _ ->
                    onlyLastSegmentMayContainBoundaryNode kind nodes.Tail
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
                
        onlyLastSegmentMayContainBoundaryNode "Target" target
        onlyLastSegmentMayContainBoundaryNode "Replacement" replacement
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
