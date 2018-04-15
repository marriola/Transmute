namespace TransmuteLib

open Exceptions
open System

type UnexpectedTokenException(expected:TokenType, got:Token) =
    inherit SyntaxException(sprintf "Expected '%s', got '%s'" (expected.ToString()) (got.tokenType.ToString()), got.position)

type ExpectedSetException(expected:TokenType list, got:Token) =
    inherit ApplicationException(
        (sprintf "Expected one of %s, got '%s' at row %d, column %d"
            (String.concat ", " (List.map string expected))
            (got.ToString()) <|| got.position))

type Node =
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

type RuleParserResult =
    | OK of Node list
    | SyntaxError of string * (int * int)

type NextResult =
    | OK of Token list * Node
    | SyntaxError of string * (int * int)

module RuleParser =
    /// <summary>
    /// Parses the next node in the list of tokens.
    /// </summary>
    /// <param name="tokens">The list of tokens.</param>
    let rec next tokens =
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
                raise (SyntaxException (sprintf "Expected '%s', got '%s'" (target.ToString()) (tokens.Head.ToString()), tokens.Head.position))

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
            tokens, FeatureIdentifierTermNode (presence.tokenType = Plus, identifier.value)

        /// <summary>
        /// Matches a <see cref="SetIdentifierTermNode" />.
        /// </summary>
        /// <param name="tokens">The list of tokens.</param>
        let matchSetIdentifierTerm tokens =
            let tokens, identifier = matchToken tokens Id
            tokens, SetIdentifierTermNode identifier.value

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
        let matchSetIdentifier tokens =
            let rec matchSetIdentifierInternal tokens out =
                match tokens with
                | [] ->
                    raise (SyntaxException ("Expected ']', got end of file", _position))
                | x::xs ->
                    match x.tokenType with
                    | RBrack ->
                        xs, SetIdentifierNode (List.rev out)
                    | _ ->
                        let tokens, term = matchSetOrFeatureIdentifierTerm tokens
                        matchSetIdentifierInternal tokens (term :: out)
            matchSetIdentifierInternal tokens []

        /// <summary>
        /// Matches a rule segment, i.e. a list of <see cref="IdentifierNode" />, <see cref="UtteranceNode" />,
        /// <see cref="PlaceholderNode" />, <see cref="BoundaryNode" />, <see cref="SetIdentifierNode" />,
        /// <see cref="OptionalNode" /> or <see cref="DisjunctNode" />.
        /// </summary>
        /// <param name="tokens">The list of tokens.</param>
        let rec matchRuleSegment tokens =
            let matchOptional tokens =
                let rec matchOptionalInternal tokens out =
                    match tokens with
                    | [] ->
                        tokens, OptionalNode (List.rev out)
                    | x::xs ->
                        match x.tokenType with
                        | RParen ->
                            xs, OptionalNode (List.rev out)
                        | _ ->
                            let tokens, ruleSegment = matchRuleSegment tokens in
                            matchOptionalInternal tokens (List.concat [ (List.rev ruleSegment); out ])
                matchOptionalInternal tokens []

            let rec matchRuleSegmentInternal tokens out =
                match tokens with
                | [] ->
                    tokens, List.rev out
                | x::xs ->
                    match x.tokenType with
                    | Id ->
                        matchRuleSegmentInternal xs (IdentifierNode x.value :: out)
                    | Utterance ->
                        matchRuleSegmentInternal xs (UtteranceNode x.value :: out)
                    | Placeholder ->
                        matchRuleSegmentInternal xs (PlaceholderNode :: out)
                    | Boundary ->
                        matchRuleSegmentInternal xs (BoundaryNode :: out)
                    | LBrack ->
                        let tokens, setIdentifier = matchSetIdentifier xs
                        matchRuleSegmentInternal tokens (setIdentifier :: out)
                    | LParen ->
                        let tokens, optional = matchOptional xs
                        matchRuleSegmentInternal tokens (optional :: out)
                    | _ ->
                        tokens, List.rev out
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
                tokens, TransformationNode (utterance, UtteranceNode result.value)
            | _ ->
                tokens, utterance

        /// <summary>
        /// Matches a list of set/feature members. These may be of type <see cref="UtteranceNode" /> or
        /// <see cref="TransformationNode" />.
        /// </summary>
        /// <param name="tokens">The list of tokens.</param>
        let matchMemberList tokens =
            let rec matchMemberListInternal tokens out =
                match tokens with
                | [] ->
                    raise (SyntaxException ("Expected member list, got end of file", _position))
                | x::xs ->
                    match x.tokenType with
                    | Whitespace ->
                        matchMemberListInternal xs out
                    | Utterance ->
                        let utterance = UtteranceNode x.value
                        let tokens, utteranceOrTransformation = matchUtterance_Transformation xs utterance
                        matchMemberListInternal tokens (utteranceOrTransformation :: out)
                    | RBrace ->
                        xs, List.rev out
                    // TODO: include members of another set/feature
                    | _ ->
                        raise (UnexpectedTokenException (Utterance, x))
            matchMemberListInternal tokens []

        /// <summary>
        /// Matches a rule.
        /// </summary>
        /// <param name="tokens">The list of tokens.</param>
        let matchRule tokens =
            let tokens, target = matchRuleSegment tokens
            let tokens, _ = matchToken tokens Divider
            let tokens, replacement = matchRuleSegment tokens
            let tokens, _ = matchToken tokens Divider
            let tokens, environment = matchRuleSegment tokens
            tokens, RuleNode (target, replacement, environment)

        /// <summary>
        /// Matches a rule when an identifier has already been matched.
        /// </summary>
        /// <param name="tokens">The list of tokens.</param>
        /// <param name="identifier">The identifier already matched.</param>
        let matchRuleStartingWithIdentifier tokens (identifier: Token) =
            let tokens, ruleNode = matchRule tokens
            match ruleNode with
            | RuleNode (target, replacement, environment) ->
                tokens, RuleNode (IdentifierNode identifier.value :: target, replacement, environment)
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
                tokens, SetDefinitionNode (identifier.value, members)
            | Utterance ->
                let tokens, ruleNode = matchRuleStartingWithIdentifier tokens identifier
                tokens, ruleNode
            | _ -> raise (SyntaxException ("unexpected error", nextToken.position))

        /// <summary>
        /// Matches a rule when a set identifier has already been matched.
        /// </summary>
        /// <param name="tokens">The list of tokens.</param>
        let matchRuleStartingWithSetIdentifier tokens =
            let tokens, setIdentifier = matchSetIdentifier tokens
            let tokens, ruleNode = matchRule tokens
            match ruleNode with
            | RuleNode (target, replacement, environment) ->
                tokens, RuleNode (setIdentifier :: target, replacement, environment)
            | _ ->
                raise (SyntaxException ("unexpected error", _position))
                    
        /// <summary>
        /// Matches a feature definition.
        /// </summary>
        /// <param name="tokens">The list of tokens.</param>
        let matchFeature tokens =
            let tokens, identifier = matchToken tokens Id
            let tokens, _ = matchToken tokens RBrack
            let tokens, _ = matchToken tokens LBrace
            let tokens, memberList = matchMemberList tokens
            tokens, FeatureDefinitionNode (identifier.value, memberList)

        /// <summary>
        /// Matches either a <see cref="FeatureDefinitionNode" />, a <see cref="SetIdentifierNode" />
        /// or a <see cref="RuleNode" />.
        /// </summary>
        /// <param name="tokens">The list of tokens.</param>
        let matchFeature_SetIdentifier_Rule (tokens: Token list) =
            match tokens.Head.tokenType with
            | Plus | Minus ->
                matchRuleStartingWithSetIdentifier tokens
            | Id ->
                matchFeature tokens
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
                    SyntaxError ("End of file", (0, 0))
                | x::xs ->
                    match x.tokenType with
                    | Whitespace ->
                        nextInternal xs
                    | Utterance ->
                        OK (matchRule tokens)
                    | Id ->
                        OK (matchSet_Rule xs x)
                    | LBrack ->
                        OK (matchFeature_SetIdentifier_Rule xs)
                    | _ ->
                        SyntaxError (sprintf "Unexpected token '%s'" x.value, x.position)
            with
                | :? SyntaxException as ex -> SyntaxError (ex.Message, (0, 0))
   
        // Store result and updat position before returning
        let result = (nextInternal tokens)

        match result with
        | OK (nextTokens, node) ->
            if [] <> nextTokens then
                _position <- nextTokens.Head.position
            result
        | _ -> result

    /// <summary>
    /// Parses a list of tokens to a list of nodes.
    /// </summary>
    /// <param name="tokens">The list of tokens to parse.</param>
    let parse tokens =
        let rec parseInternal tokens (out: Node list) =
            match tokens with
            | [] ->
                List.rev out
            | _ ->
                let result = next tokens
                match result with
                | OK (nextTokens, node) ->
                    parseInternal nextTokens (node :: out)
                | SyntaxError (message, position) ->
                    raise (SyntaxException (message, position))
        parseInternal tokens []
