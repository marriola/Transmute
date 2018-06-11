namespace TransmuteLib

type Node =
    /// Represents a comment.
    | CommentNode of string

    /// Represents an identifier.
    | IdentifierNode of string

    /// Defines the intersection of a list of sets and features.
    | SetIdentifierNode of Node list

    /// Represents the presence of a set in a set identifier.
    | SetIdentifierTermNode of name:string

    /// Represents the presenc or absence of a feature in a set identifier.
    | FeatureIdentifierTermNode of isPresent:bool * name:string

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

/// Provides functions on the Node type.
module Node =
    open System

    let tag node position =
        TaggedNode (position, node)

    /// Retrieves the inner node of a tagged node.
    let inline untag taggedNode =
        match taggedNode with
        | TaggedNode (_, node) ->
            node
        | x ->
            x

    /// Retrieves the metadata and inner node of a tagged node.
    let inline untagWithMetadata taggedNode =
        match taggedNode with
        | TaggedNode (position, node) ->
            position, node
        | x ->
            (0, 0), x

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
        | IdentifierNode value
        | SetIdentifierTermNode value ->
            value
        | FeatureIdentifierTermNode (_, name) ->
            name
        | SetDefinitionNode (name, _) 
        | FeatureDefinitionNode (name, _) ->
            name
        | _ ->
            invalidArg "this" "Must be one of UtteranceNode, CommentNode, IdentifierNode, SetIdentifierTermNode"

    /// <summary>
    /// Returns a dictionary of the elements of the Node list that are FeatureDefinitionNodes.
    /// </summary>
    /// <param name="nodes"></param>
    let getFeatures nodes =
        nodes
        |> List.choose
            (fun x ->
                match untag x with
                | FeatureDefinitionNode (name, members) ->
                    Some (name, FeatureDefinitionNode (name, members))
                | _ -> None)
        |> dict

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
        |> dict

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
                let next = 
                    match untag x with
                    | UtteranceNode value ->
                        value
                    | TransformationNode (target, result) ->
                        let utterance = if isPresent then result else target
                        match untag utterance with
                        | UtteranceNode value -> value
                    | x ->
                        let position, node = untagWithMetadata x
                        Exceptions.invalidSyntax position (sprintf "Unrecognized token '%s'" (string node))
                inner xs (next :: out)
        match feature with
        | FeatureDefinitionNode (_, members) ->
            inner members []
        | _ ->
            invalidArg "feature" "Must be a FeatureDefinitionNode"

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
        [ List.map (getFeatureMembers true) features;
          List.map (getFeatureMembers false) features;
          List.map getSetMembers sets ]
        |> List.map List.concat
        |> List.concat
        |> set

