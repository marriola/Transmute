namespace TransmuteLib

open System.Collections.Generic

type Node =
    /// Represents a comment.
    | CommentNode of string

    /// Represents an identifier.
    | SetIdentifierNode of string

    /// Defines the intersection of a list of sets and features.
    | CompoundSetIdentifierNode of Node list

    /// Represents the presence of a set in a set identifier.
    | TermIdentifierNode of name:string

    /// Represents the presence or absence of a feature in a set identifier.
    | FeatureIdentifierNode of isPresent:bool * name:string

    /// Represents an utterance.
    | UtteranceNode of string

    /// Represents the placeholder for the target segment in the environment segment.
    | PlaceholderNode

    /// Represents a word boundary in the environment segment.
    | BoundaryNode

    /// Represents a phonological rule.
    | RuleNode of target:Node list * result:Node list * environment:Node list

    /// Defines a set of utterances.
    | SetDefinitionNode of name:string * members:Node list

    /// Defines a transformation from one phoneme to another.
    | TransformationNode of target:Node * result:Node

    /// Defines a set of phonemes possessing this feature and transformations on it.
    | FeatureDefinitionNode of name:string * members:Node list

    /// Defines a list of nodes that may be optionally matched.
    | OptionalNode of Node list

    /// Defines a set of lists of nodes, only one of which may be matched.
    | DisjunctNode of Node list list

    /// Represents a node tagged with metadata.
    | TaggedNode of pos:(int * int) * Node

    with
    override this.ToString() =
        match this with
        | TaggedNode (_, node) -> node.ToString()
        | PlaceholderNode -> "_"
        | BoundaryNode -> "#"
        | CommentNode text -> sprintf "; %s" text
        | UtteranceNode value
        | SetIdentifierNode value
        | TermIdentifierNode value -> value
        | FeatureIdentifierNode (isPresent, name) ->
            let sign = if isPresent then "+" else "-"
            sprintf "%s%s" sign name
        | CompoundSetIdentifierNode terms ->
            terms
            |> List.map string
            |> String.concat ""
            |> sprintf "[%s]"
        | SetDefinitionNode (name, members) ->
            members
            |> List.map string
            |> String.concat " "
            |> sprintf "%s { %s }" name
        | TransformationNode (target, result) ->
            sprintf "%s => %s" (string target) (string result)
        | FeatureDefinitionNode (name, members) ->
            members 
            |> List.map string
            |> String.concat "; "
            |> sprintf "[%s] { %s }" name
        | OptionalNode children ->
            children
            |> List.map string
            |> String.concat ""
            |> sprintf "(%s)"
        | DisjunctNode branches ->
            branches
            |> List.collect (List.map string)
            |> String.concat "|"
            |> sprintf "(%s)"
        | RuleNode (target, replacement, environment) ->
            sprintf "%s/%s/%s"
                (target |> List.map string |> String.concat "")
                (replacement |> List.map string |> String.concat "")
                (environment |> List.map string |> String.concat "")

/// Provides functions on the Node type.
module Node =
    let tag node position =
        TaggedNode (position, node)

    /// Retrieves the inner node of a tagged node.
    let inline untag taggedNode =
        match taggedNode with
        | TaggedNode (_, node) ->
            node
        | x ->
            x

    let (|Untag|_|) node =
        match node with
        | TaggedNode (position, inner) -> Some (inner, position)
        | _ -> Some (node, (0, 0))

    /// Gets the left string value of a TransformationNode.
    let getLeft node =
        match untag node with
        | TransformationNode (target, _) ->
            target
        | _ ->
            invalidArg "this" "Must be a TranformationNode"

    /// Gets the right string value of a TransformationNode.
    let getRight node =
        match untag node with
        | TransformationNode (_, result)->
            result
        | _ ->
            invalidArg "this" "Must be a TranformationNode"

    /// Gets the string value of a node.
    let getStringValue node =
        match untag node with
        | UtteranceNode value
        | CommentNode value
        | SetIdentifierNode value
        | TermIdentifierNode value ->
            value
        | FeatureIdentifierNode (_, name) ->
            name
        | SetDefinitionNode (name, _) 
        | FeatureDefinitionNode (name, _) ->
            name
        | _ ->
            invalidArg "this" "Must be one of UtteranceNode, CommentNode, SetIdentifierNode, TermIdentifierNode"

    /// <summary>
    /// Returns a dictionary of the elements of the Node list that are FeatureDefinitionNodes.
    /// </summary>
    /// <param name="nodes"></param>
    let getFeatures nodes =
        nodes
        |> List.choose
            (fun x ->
                match untag x with
                | FeatureDefinitionNode (name, _) as node ->
                    Some (name, node)
                | _ -> None)
        |> Map.ofSeq

    let getMembers feature =
        match feature with
        | FeatureDefinitionNode (_, members) ->
            members
        | _ ->
            invalidArg "feature" "Must be a FeatureDefinitionNode"

    /// <summary>
    /// Returns a dictionary of the elements of the Node list that are SetDefinitionNodes.
    /// </summary>
    /// <param name="nodes"></param>
    let getSets nodes =
        nodes
        |> List.choose
            (fun x ->
                match untag x with
                | SetDefinitionNode (name, members) ->
                    Some (name, SetDefinitionNode (name, members))
                | _ -> None)
        |> Map.ofSeq

    /// <summary>
    /// Gets the members of the feature.
    /// </summary>
    /// <param name="feature">The feature.</param>
    /// <param name="isPresent">If true, takes the right hand side from each transformation; otherwise, takes the left hand side.</param>
    let getFeatureMembers isPresent feature =
        let rec inner members out =
            match members with
            | [] ->
                List.rev out
            | x::xs ->
                let nextOut = 
                    match x with
                    | Untag (UtteranceNode value, _) when isPresent ->
                        value :: out
                    | Untag (TransformationNode (target, result), _) ->
                        let utterance = if isPresent then result else target
                        match utterance with
                        | Untag (UtteranceNode value, _) -> value :: out
                    | _ ->
                        out
                inner xs nextOut
        inner (getMembers feature) []

    type private Transformation =
        | Add of target: string * result: string
        | Remove of target: string * result: string

    /// Returns a map of phonemes that can be transformed to add the feature, and
    /// a map of phonemes that can be transformed to remove the feature.
    let getTransformations feature =
        let transformations =
            feature
            |> getMembers
            |> List.choose (fun m ->
                match m with
                | TaggedNode (_, TransformationNode (target, result)) ->
                    let target = getStringValue target
                    let result = getStringValue result
                    Some [
                        Add (target, result)
                        Remove (result, target)
                    ]
                | _ -> None)
            |> List.concat
        let additions =
            transformations
            |> List.choose (function
                | Add (target, result) -> Some (target, result)
                | Remove _ -> None)
            |> Map.ofSeq
        let removals =
            transformations
            |> List.choose (function
                | Remove (target, result) -> Some (target, result)
                | Add _ -> None)
            |> Map.ofSeq
        additions, removals

    /// <summary>
    /// Gets the members of the set.
    /// </summary>
    /// <param name="theSet"></param>
    let getSetMembers setNode =
        match untag setNode with
        | SetDefinitionNode (_, members) ->
            List.map (fun x -> getStringValue x) members
        | _ ->
            invalidArg "setNode" "Must be a set"

    let getAlphabet features sets =
        [ List.map (getFeatureMembers true) features
          List.map (getFeatureMembers false) features
          List.map getSetMembers sets ]
        |> List.collect List.concat
        |> set

    /// <summary>
    /// Tries to execute a function that retrieves a type of object (set, feature, etc.). If the function
    /// throws a KeyNotFoundException, throws a SyntaxException.
    /// </summary>
    /// <param name="fn">The function to try.</param>
    /// <param name="kind">The kind of object being retrieved.</param>
    /// <param name="node">The node naming the object to retrieve.</param>
    /// <param name="name">The name of the object being retireved.</param>
    let private tryFindSetOrFeature fn kind node name =
        try fn()
        with
            | :? KeyNotFoundException ->
                let msg = sprintf "'%s' '%s' not defined" kind name
                match node with
                | TaggedNode (pos, _) -> invalidSyntax msg pos
                | _ -> invalidSyntax msg (1, 1)

    /// <summary>
    /// Computes the intersection of the sets and features named in the CompoundSetIdentifierNode.
    /// </summary>
    /// <param name="sets">The available sets.</param>
    /// <param name="features">The available features.</param>
    /// <param name="setIdentifier"></param>
    let setIntersection (alphabet: Set<string>) (features: Map<string, Node>) (sets: Map<string, Node>) setIdentifier =
        let rec inner (terms: Node list) (result: Set<string>) =
            let addToSet isPresent s =
                if isPresent
                    then Set.intersect result s
                    else Set.difference result s

            match terms with
            | [] ->
                result
            | x::xs ->
                let nextSet =
                    match x with
                    | Untag (TermIdentifierNode name, _)
                    | Untag (SetIdentifierNode name, _) ->
                        tryFindSetOrFeature (fun _ -> getSetMembers sets.[name]) "Set" x name
                        |> set
                        |> addToSet true
                    | Untag (FeatureIdentifierNode (isPresent, name), _) ->
                        if features.ContainsKey(name) then
                            getFeatureMembers isPresent features.[name]
                            |> set
                            |> Set.intersect result
                        elif sets.ContainsKey(name) then
                            let setMembers = getSetMembers sets.[name] |> set
                            if isPresent
                                then Set.intersect result setMembers
                                else Set.difference result setMembers
                        else
                            failwithf "%s is not defined" name
                    | Untag (node, position) ->
                        invalidSyntax (sprintf "Unexpected token '%O'" node) position
                inner xs nextSet
        inner setIdentifier alphabet |> List.ofSeq
