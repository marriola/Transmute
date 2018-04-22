namespace TransmuteLib

open Exceptions
open System

type UnexpectedTokenException(expected:TokenType, got:Token) =
    inherit SyntaxException(sprintf "Expected '%s', got '%s'" (expected.ToString()) (got.tokenType.ToString()), got.position)

type ExpectedSetException(expected:TokenType list, got:Token) =
    inherit ApplicationException(
        (sprintf "Expected one of %s, got '%s' at row %d, column %d"
            (String.concat ", " (List.map string expected))
            (got.tokenType.ToString()) <|| got.position))

type Node =
    /// <summary>
    /// Represents a comment.
    /// </summary>
    | CommentNode of string

    /// <summary>
    /// Represents an identifier.
    /// </summary>
    | IdentifierNode of string

    /// <summary>
    /// Defines the intersection of a list of sets and features.
    /// </summary>
    | SetIdentifierNode of Node list

    /// <summary>
    /// Represents the presence of a set in a set identifier.
    /// </summary>
    | SetIdentifierTermNode of name:string

    /// <summary>
    /// Represents the presenc or absence of a feature in a set identifier.
    /// </summary>
    | FeatureIdentifierTermNode of isPresent:bool * name:string

    /// <summary>
    /// Represents an utterance.
    /// </summary>
    | UtteranceNode of string

    /// <summary>
    /// Represents the placeholder for the target segment in the environment segment.
    /// </summary>
    | PlaceholderNode

    /// <summary>
    /// Represents a word boundary in the environment segment.
    /// </summary>
    | BoundaryNode

    /// <summary>
    /// Represents a phonological rule.
    /// </summary>
    | RuleNode of target:Node list * result:Node list * environment:Node list

    /// <summary>
    /// Defines a set of utterances.
    /// </summary>
    | SetDefinitionNode of name:string * members:Node list

    /// <summary>
    /// Defines a transformation from one phoneme to another.
    /// </summary>
    | TransformationNode of target:Node * result:Node

    /// <summary>
    /// Defines a set of phonemes possessing this feature and transformations on it.
    /// </summary>
    | FeatureDefinitionNode of name:string * members:Node list

    /// <summary>
    /// Defines a list of nodes that may be optionally matched.
    /// </summary>
    | OptionalNode of Node list

    /// <summary>
    /// Defines a set of lists of nodes, only one of which may be matched.
    /// </summary>
    | DisjunctNode of Node list

    /// <summary>
    /// Represents a node tagged with metadata.
    /// </summary>
    | TaggedNode of pos:(int * int) * Node

type RuleParserResult =
    | OK of Node list
    | SyntaxError of string * (int * int)

type NextResult =
    | OK of Token list * Node
    | SyntaxError of string * (int * int)

type ValidateResult =
    | OK
    | SyntaxError of string * (int * int)

module RuleParser =
    open System.Collections.Generic

    let inline tag node position =
        TaggedNode (position, node)

    let inline untag taggedNode =
        match taggedNode with
        | TaggedNode (_, node) ->
            node
        | x ->
            x

    let inline untagWithMetadata taggedNode =
        match taggedNode with
        | TaggedNode (position, node) ->
            position, node
        | x ->
            (0, 0), x

    /// <summary>
    /// Gets the members of the feature.
    /// </summary>
    /// <param name="feature">The feature.</param>
    /// <param name="isPresent">If true, takes the right hand side from each transformation; otherwise, takes the left hand side.</param>
    let getFeatureMembers feature isPresent =
        let rec inner (members: Node list) out =
            if members.IsEmpty then
                List.rev out
            else 
                let next = 
                    match untag members.Head with
                    | UtteranceNode value ->
                        value
                    | TransformationNode (target, result) ->
                        let utterance = if isPresent then result else target
                        match untag utterance with
                        | UtteranceNode value -> value
                    | x ->
                        let position, node = untagWithMetadata members.Head
                        raise (SyntaxException (sprintf "Unrecognized token '%s'" (string node), position))
                inner members.Tail (next :: out)
        match feature with
        | FeatureDefinitionNode (_, members) ->
            inner members []
        | _ ->
            raise (ArgumentException ("Must be a FeatureDefinitionNode", "feature"))

    /// <summary>
    /// Gets the members of the set.
    /// </summary>
    /// <param name="theSet"></param>
    let getSetMembers theSet =
        match untag theSet with
        | SetDefinitionNode (_, members) ->
            members
                |> List.map (fun x ->
                    match untag x with
                    | UtteranceNode value -> value
                    | _ ->
                        let position, node = untagWithMetadata x
                        raise (SyntaxException (sprintf "Unexpected token '%s'" (string node), position)))
        | _ ->
            raise (ArgumentException ("Must be a set", "theSet"))

    let private makeSyntaxError node format =
        let position, innerNode = untagWithMetadata node
        raise (SyntaxException (format position innerNode, position))

    let private undefinedSetOrFeature kind name position node =
        sprintf "'%s' '%s' not defined" kind name

    let private tryFindSetOrFeature fn kind node name =
        try fn()
        with
            | :? KeyNotFoundException ->
                undefinedSetOrFeature kind name
                    |> makeSyntaxError node
                    |> raise

    /// <summary>
    /// Computes the intersection of the sets and features named in the SetIdentifierNode.
    /// </summary>
    /// <param name="sets">The available sets.</param>
    /// <param name="features">The available features.</param>
    /// <param name="setIdentifier"></param>
    let setIntersection (sets: IDictionary<string, Node>) (features: IDictionary<string, Node>) setIdentifier =
        let rec inner (terms: Node list) (result: Set<string>) =
            if terms.IsEmpty then
                result
            else
                let setMembers =
                    match untag terms.Head with
                    | IdentifierNode name ->
                        tryFindSetOrFeature (fun _ -> getSetMembers sets.[name]) "Set" terms.Head name |> set
                    | FeatureIdentifierTermNode (isPresent, name) ->
                        tryFindSetOrFeature (fun _ -> getFeatureMembers features.[name] isPresent) "Feature" terms.Head name |> set
                    | _ ->
                        let position, node = untagWithMetadata terms.Head
                        raise (SyntaxException (sprintf "Unexpected token '%s'" (string node), position))
                let nextSet =
                    if result.IsEmpty then
                        setMembers
                    else
                        let differenceRight = result - setMembers
                        let differenceLeft = setMembers - result
                        result - differenceLeft - differenceRight
                inner terms.Tail nextSet
        match untag setIdentifier with
        | SetIdentifierNode terms ->
            inner terms (set []) |> List.ofSeq
        | _ ->
            raise (ArgumentException ("Must be a SetIdentifierNode", "setIdentifier"))

    /// <summary>
    /// Parses the next node in the list of tokens.
    /// </summary>
    /// <param name="tokens">The list of tokens.</param>
    let next tokens =
        let mutable _position = (1, 1)

        /// <summary>
        /// Matches a token to a specific type.
        /// <summary>
        /// <exception cref="SyntaxException">Thrown when the token at the head of the list does not match the given type.</exception>
        let rec matchToken (tokens: Token list) (target: TokenType) =
            let tokenType = tokens.Head.tokenType
            if tokenType = Whitespace then
                matchToken tokens.Tail target
            else if tokenType = target then
                tokens.Tail, tokens.Head
            else
                raise (UnexpectedTokenException (target, tokens.Head))

        /// <summary>
        /// Matches a token to one of a set of token types.
        /// </summary>
        /// <param name="tokens">The list of tokens.</param>
        /// <param name="targetTypes">The list of valid token types.</param>
        let rec matchSet (tokens: Token list) (targetTypes: TokenType list) =
            let tokenType = tokens.Head.tokenType
            if tokenType = Whitespace then
                matchSet tokens.Tail targetTypes
            else if List.contains tokenType targetTypes then
                tokens.Tail, tokens.Head
            else
                raise (ExpectedSetException (targetTypes, tokens.Head))

        /// <summary>
        /// Matches a <see cref="FeatureIdentifierTermNode" />.
        /// </summary>
        /// <param name="tokens">The list of tokens.</param>
        let matchFeatureIdentifierTerm tokens =
            let tokens, presence = matchSet tokens [ Plus; Minus ]
            let tokens, identifier = matchToken tokens Id
            tokens, tag (FeatureIdentifierTermNode (presence.tokenType = Plus, identifier.value)) presence.position

        /// <summary>
        /// Matches a <see cref="SetIdentifierTermNode" />.
        /// </summary>
        /// <param name="tokens">The list of tokens.</param>
        let matchSetIdentifierTerm tokens =
            let tokens, identifier = matchToken tokens Id
            tokens, tag (SetIdentifierTermNode identifier.value) identifier.position

        /// <summary>
        /// Matches either a <see cref="SetIdentifierTermNode" /> or a <see cref="FeatureIdentifierTermNode" />.
        /// </summary>
        /// <param name="tokens">The list of tokens.</param>
        let matchSetOrFeatureIdentifierTerm (tokens: Token list) =
            match tokens.Head.tokenType with
            | Id ->
                matchSetIdentifierTerm tokens
            | Plus | Minus ->
                matchFeatureIdentifierTerm tokens
            | _ ->
                raise (ExpectedSetException ([ Id; Plus; Minus ], tokens.Head))

        /// <summary>
        /// Matches a <see cref="SetIdentifierNode" />.
        /// </summary>
        /// <param name="tokens">The list of tokens.</param>
        let matchSetIdentifier (tokens: Token list) headPosition =
            let rec matchSetIdentifierInternal tokens result =
                match tokens with
                | [] ->
                    raise (SyntaxException ("Expected ']', got end of file", _position))
                | x::xs ->
                    match x.tokenType with
                    | RBrack ->
                        xs, tag (SetIdentifierNode (List.rev result)) headPosition
                    | _ ->
                        let tokens, term = matchSetOrFeatureIdentifierTerm tokens
                        matchSetIdentifierInternal tokens (term :: result)
            matchSetIdentifierInternal tokens []

        /// <summary>
        /// Matches a rule segment, i.e. a list of <see cref="IdentifierNode" />, <see cref="UtteranceNode" />,
        /// <see cref="PlaceholderNode" />, <see cref="BoundaryNode" />, <see cref="SetIdentifierNode" />,
        /// <see cref="OptionalNode" /> or <see cref="DisjunctNode" />.
        /// </summary>
        /// <param name="tokens">The list of tokens.</param>
        let rec matchRuleSegment tokens =
            /// <summary>
            /// Matches a <see cref="DisjunctNode" />.
            /// </summary>
            /// <param name="tokens">The list of tokens.</param>
            /// <param name="result">The contents of the node.</param>
            let rec matchDisjunct (tokens: Token list) (startToken: Token) result =
                match tokens.Head.tokenType with
                | Whitespace ->
                    matchDisjunct tokens.Tail startToken result
                | RParen ->
                    tokens.Tail, tag (DisjunctNode (List.rev result)) startToken.position
                | _ ->
                    let tokens, ruleSegment = matchRuleSegment tokens in
                    matchDisjunct tokens startToken (List.concat [ (List.rev ruleSegment); result ])

            /// <summary>
            /// Matches either an <see cref="OptionalNode" /> or a <see cref="DisjunctNode" />.
            /// </summary>
            /// <param name="tokens">The list of tokens.</param>
            let matchOptional_Disjunct tokens =
                let tokens, lparen = matchToken tokens LParen
                let rec matchOptional_DisjunctInteral (tokens: Token list) result =
                    match tokens.Head.tokenType with
                    | Whitespace ->
                        matchOptional_DisjunctInteral tokens.Tail result
                    | RParen ->
                        tokens.Tail, tag (OptionalNode (List.rev result)) lparen.position
                    | Pipe ->
                        matchDisjunct tokens.Tail lparen result
                    | _ ->
                        let tokens, ruleSegment = matchRuleSegment tokens in
                        matchOptional_DisjunctInteral tokens (List.concat [ (List.rev ruleSegment); result ])
                matchOptional_DisjunctInteral tokens []

            let rec matchRuleSegmentInternal tokens result =
                match tokens with
                | [] ->
                    tokens, List.rev result
                | x::xs ->
                    match x.tokenType with
                    | Separator ->
                        matchRuleSegmentInternal xs result
                    | Id ->
                        matchRuleSegmentInternal xs (tag (IdentifierNode x.value) x.position :: result)
                    | Utterance ->
                        matchRuleSegmentInternal xs (tag (UtteranceNode x.value) x.position :: result)
                    | Placeholder ->
                        matchRuleSegmentInternal xs (tag PlaceholderNode x.position :: result)
                    | Boundary ->
                        matchRuleSegmentInternal xs (tag BoundaryNode x.position :: result)
                    | LBrack ->
                        let tokens, setIdentifier = matchSetIdentifier xs x.position
                        matchRuleSegmentInternal tokens (setIdentifier :: result)
                    | LParen ->
                        let tokens, optional = matchOptional_Disjunct tokens
                        matchRuleSegmentInternal tokens (optional :: result)
                    | _ ->
                        tokens, List.rev result
            matchRuleSegmentInternal tokens []

        /// <summary>
        /// Matches either an <see cref="UtteranceNode" /> or a <see cref="TransformationNode" />.
        /// </summary>
        /// <param name="tokens">The list of tokens.</param>
        /// <param name="utterance">The utterance already matched.</param>
        let rec matchUtterance_Transformation (tokens: Token list) utterance =
            match tokens.Head.tokenType with
            | Whitespace ->
                matchUtterance_Transformation tokens.Tail utterance
            | Gives ->
                let tokens, result = matchToken tokens.Tail Utterance
                tokens, tag (TransformationNode (
                    tag (UtteranceNode utterance.value) utterance.position,
                    tag (UtteranceNode result.value) result.position)) utterance.position
            | _ ->
                tokens, tag (UtteranceNode utterance.value) utterance.position

        /// <summary>
        /// Matches a list of set/feature members. These may be of type <see cref="UtteranceNode" /> or
        /// <see cref="TransformationNode" />.
        /// </summary>
        /// <param name="tokens">The list of tokens.</param>
        let matchMemberList tokens =
            let rec matchMemberListInternal tokens result =
                match tokens with
                | [] ->
                    raise (SyntaxException ("Expected member list, got end of file", _position))
                | x::xs ->
                    match x.tokenType with
                    | Whitespace ->
                        matchMemberListInternal xs result
                    | Utterance ->
                        let tokens, utteranceOrTransformation = matchUtterance_Transformation xs x
                        matchMemberListInternal tokens (utteranceOrTransformation :: result)
                    | RBrace ->
                        xs, List.rev result
                    // TODO: include members of another set/feature
                    | _ ->
                        raise (UnexpectedTokenException (Utterance, x))
            matchMemberListInternal tokens []

        /// <summary>
        /// Matches a rule.
        /// </summary>
        /// <param name="tokens">The list of tokens.</param>
        let matchRule tokens headPosition =
            let tokens, target = matchRuleSegment tokens
            let tokens, _ = matchToken tokens Divider
            let tokens, replacement = matchRuleSegment tokens
            let tokens, _ = matchToken tokens Divider
            let tokens, environment = matchRuleSegment tokens
            tokens, tag (RuleNode (target, replacement, environment)) headPosition

        /// <summary>
        /// Matches a rule when an identifier has already been matched.
        /// </summary>
        /// <param name="tokens">The list of tokens.</param>
        /// <param name="identifier">The identifier already matched.</param>
        let matchRuleStartingWithIdentifier tokens (identifier: Token) =
            let tokens, ruleNode = matchRule tokens identifier.position
            match untag ruleNode with
            | RuleNode (target, replacement, environment) ->
                tokens, tag
                    (RuleNode (
                        tag (IdentifierNode identifier.value) identifier.position :: target,
                        replacement,
                        environment))
                    identifier.position
            | _ -> raise (SyntaxException ("unexpected error", _position))

        /// <summary>
        /// Matches either a set or a rule.
        /// </summary>
        /// <param name="tokens">The list of tokens.</param>
        let matchSet_Rule tokens identifier =
            let tokens, nextToken = matchSet tokens [ LBrace; Utterance ]
            match nextToken.tokenType with
            | LBrace ->
                // TODO: include members of other sets
                let tokens, members = matchMemberList tokens
                tokens, tag (SetDefinitionNode (identifier.value, members)) identifier.position
            | Utterance ->
                let tokens, ruleNode = matchRuleStartingWithIdentifier tokens identifier
                tokens, ruleNode
            | _ -> raise (SyntaxException ("unexpected error", nextToken.position))

        /// <summary>
        /// Matches a rule when a set identifier has already been matched.
        /// </summary>
        /// <param name="tokens">The list of tokens.</param>
        let matchRuleStartingWithSetIdentifier tokens headPosition =
            let tokens, setIdentifier = matchSetIdentifier tokens headPosition
            let tokens, ruleNode = matchRule tokens headPosition
            match untag ruleNode with
            | RuleNode (target, replacement, environment) ->
                tokens, tag (RuleNode (setIdentifier :: target, replacement, environment)) headPosition
            | _ ->
                raise (SyntaxException ("unexpected error", _position))

        /// <summary>
        /// Prepends a node list to the target segment of a RuleNode.
        /// </summary>
        /// <exception cref="System.ArgumentException">Thrown when the argument to <c>rule<c/>
        /// is not a <see cref="RuleNode" />.</exception>
        let prependToRule rule headPosition (nodes: Node list) =
            match untag rule with
            | RuleNode (target, replacement, environment) ->
                tag (RuleNode (List.concat [ nodes; target ], replacement, environment)) headPosition
            | _ ->
                raise (ArgumentException ("Must be a RuleNode", "rule"))

        /// <summary>
        /// Prepends a node list to the initial SetIdentifierNode of the target segment of a RuleNode.
        /// </summary>
        /// <exception cref="System.ArgumentException">Thrown when the argument to <c>rule<c/> is not a <see cref="RuleNode" />,
        /// or when the first element of the target segment is not a <see cref="SetIdentifierNode" />.</exception>
        let prependToRuleSetIdentifier rule headPosition (nodes: Node list) =
            match untag rule with
            | RuleNode (target, _relacement, _environment) ->
                match untag target.Head with
                | SetIdentifierNode identifiers ->
                    tag
                        (RuleNode (
                            tag (SetIdentifierNode (List.concat [ nodes; identifiers ])) headPosition :: target.Tail,
                            _relacement,
                            _environment))
                        headPosition
                | _ ->
                    raise (ArgumentException ("First element of the target segment must be a SetIdentifierNode", "rule"))
            | _ ->
                raise (ArgumentException ("Must be a RuleNode", "rule"))
                    
        /// <summary>
        /// Matches a feature definition.
        /// </summary>
        /// <param name="tokens">The list of tokens.</param>
        let matchFeature tokens headPosition identifier =
            let tokens, _ = matchToken tokens RBrack
            let tokens, _ = matchToken tokens LBrace
            let tokens, memberList = matchMemberList tokens
            tokens, tag (FeatureDefinitionNode (identifier.value, memberList)) headPosition

        /// <summary>
        /// Matches either a <see cref="FeatureDefinitionNode" />, a <see cref="SetIdentifierNode" />
        /// or a <see cref="RuleNode" />.
        /// </summary>
        /// <remarks>
        /// LBrack '[' is parsed just before entering this function.
        /// </remarks>
        /// <param name="tokens">The list of tokens.</param>
        let rec matchFeature_SetIdentifier_Rule (tokens: Token list) headPosition (identifier:Token option) =
            match tokens.Head.tokenType with
            | Plus | Minus ->
                let tokens, theSet = matchRuleStartingWithSetIdentifier tokens headPosition
                match identifier with
                | Some i ->
                    // '[' Id [ '+' | '-' ] -> RuleNode (id :: target, replacement, environment)
                    tokens, prependToRuleSetIdentifier theSet headPosition [ tag (IdentifierNode i.value) i.position ]
                | None ->
                    // '[' [ '+' | '-' ] -> RuleNode (...)
                    tokens, theSet
            | RBrack ->
                match identifier with
                | Some i ->
                    // '[' Id ']' -> FeatureDefinitionNode Id.name nodeList
                    matchFeature tokens headPosition i
                | None ->
                    // '[' ']' -> syntax error
                    raise (UnexpectedTokenException (Id, tokens.Head))
            | Id ->
                match identifier with
                | Some i ->
                    // '[' Id Id -> RuleNode ((Id :: (Id :: setIdentifier)) :: target.Tail, replacement, environment)
                    let tokens, ruleNode = matchRule tokens tokens.Head.position
                    tokens, prependToRule ruleNode headPosition
                        [ tag (IdentifierNode i.value) i.position
                          tag (IdentifierNode tokens.Head.value) tokens.Head.position
                        ]
                | None ->
                    // Store first identifier and see what we get next
                    matchFeature_SetIdentifier_Rule tokens.Tail headPosition (Some tokens.Head)
            | _ ->
                raise (ExpectedSetException ([ Plus; Minus; Id ], tokens.Head))

        /// <summary>
        /// Determines which rule to match to the available tokens.
        /// </summary>
        /// <param name="tokens">The list of tokens.</param>
        /// <exception cref="SyntaxException">No rule matches the available tokens.</exception>
        let rec nextInternal tokens =
            try
                match tokens with
                | [] ->
                    NextResult.SyntaxError ("End of file", _position)
                | x::xs ->
                    match x.tokenType with
                    | Whitespace ->
                        nextInternal xs
                    | Comment ->
                        NextResult.OK (xs, tag (CommentNode x.value) x.position)
                    | Utterance ->
                        NextResult.OK (matchRule tokens x.position)
                    | Id ->
                        NextResult.OK (matchSet_Rule xs x)
                    | LBrack ->
                        NextResult.OK (matchFeature_SetIdentifier_Rule xs x.position None)
                    | _ ->
                        NextResult.SyntaxError (sprintf "Unexpected token '%s'" x.value, x.position)
            with
                | :? SyntaxException as ex ->
                    NextResult.SyntaxError (ex.Message, _position)
   
        // Store result and update position before returning
        let result = (nextInternal tokens)

        match result with
        | NextResult.OK (nextTokens, node) ->
            if [] <> nextTokens then
                _position <- nextTokens.Head.position
            result
        | _ -> result

    /// <summary>
    /// Parses a list of tokens to a list of nodes.
    /// </summary>
    /// <param name="tokens">The list of tokens to parse.</param>
    let parse tokens =
        let rec parseInternal tokens (result: Node list) =
            match tokens with
            | [] ->
                List.rev result
            | _ ->
                let nextToken = next tokens
                match nextToken with
                | NextResult.OK (nextTokens, node) ->
                    parseInternal nextTokens (node :: result)
                | NextResult.SyntaxError (message, position) ->
                    raise (SyntaxException (message, position))
        parseInternal tokens []

    let private filterMap (fn: 'a -> (string * 'a) option) nodes =
        nodes
            |> List.map fn
            |> List.filter ((<>) None)
            |> List.map Option.get

    /// <summary>
    /// Returns a dictionary of the elements of the Node list that are FeatureDefinitionNodes.
    /// </summary>
    /// <param name="nodes"></param>
    let getFeatures nodes =
        dict
            (filterMap
                (fun x ->
                    match untag x with
                    | FeatureDefinitionNode (name, members) ->
                        Some (name, FeatureDefinitionNode (name, members))
                    | _ -> None)
                nodes)

    /// <summary>
    /// Returns a dictionary of the elements of the Node list that are SetDefinitionNodes.
    /// </summary>
    /// <param name="nodes"></param>
    let getSets nodes =
        dict
            (filterMap
                (fun x ->
                    match untag x with
                    | SetDefinitionNode (name, members) ->
                        Some (name, SetDefinitionNode (name, members))
                    | _ -> None)
                nodes)
