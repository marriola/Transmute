namespace TransmuteLib

open TransmuteLib.Exceptions
open TransmuteLib.Node
open TransmuteLib.Token
open System

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
        let rec matchToken tokens target =
            match tokens with
            | [] ->
                invalidArg "tokens" "Must not be empty"
            | OfType Whitespace _::xs ->
                matchToken xs target
            | { tokenType = tokenType } as x::xs ->
                if tokenType = target then
                    xs, x
                else
                    unexpectedToken [target] x

        /// <summary>
        /// Matches a token to one of a set of token types.
        /// </summary>
        /// <param name="tokens">The list of tokens.</param>
        /// <param name="targetTypes">The list of valid token types.</param>
        let rec matchOneOf tokens targetTypes =
            match tokens with
            | [] ->
                invalidArg "tokens" "Must not be empty"
            | OfType Whitespace _::xs ->
                matchOneOf xs targetTypes
            | { tokenType = tokenType } as x::xs ->
                if List.contains tokenType targetTypes then
                    xs, x
                else
                    unexpectedToken targetTypes x

        /// <summary>
        /// Matches a <see cref="FeatureIdentifierTermNode" />.
        /// </summary>
        /// <param name="tokens">The list of tokens.</param>
        let matchFeatureIdentifierTerm tokens =
            let tokens, presence = matchOneOf tokens [ Plus; Minus ]
            let tokens, identifier = matchToken tokens Id
            tokens, Node.tag (FeatureIdentifierTermNode (presence.tokenType = Plus, identifier.value)) presence.position

        /// <summary>
        /// Matches a <see cref="SetIdentifierTermNode" />.
        /// </summary>
        /// <param name="tokens">The list of tokens.</param>
        let matchSetIdentifierTerm tokens =
            let tokens, identifier = matchToken tokens Id
            tokens, Node.tag (SetIdentifierTermNode identifier.value) identifier.position

        /// <summary>
        /// Matches either a <see cref="SetIdentifierTermNode" /> or a <see cref="FeatureIdentifierTermNode" />.
        /// </summary>
        /// <param name="tokens">The list of tokens.</param>
        let matchSetOrFeatureIdentifierTerm tokens =
            match tokens with
            | [] ->
                raise (SyntaxException ("Expected identifier, '+' or '-', got end of file", _position))
            | OfType Id _::_ ->
                matchSetIdentifierTerm tokens
            | OfType Plus _::_
            | OfType Minus _::_ ->
                matchFeatureIdentifierTerm tokens
            | x::_ ->
                unexpectedToken [ Id; Plus; Minus ] x

        /// <summary>
        /// Matches a <see cref="SetIdentifierNode" />.
        /// </summary>
        /// <param name="tokens">The list of tokens.</param>
        let matchSetIdentifier tokens headPosition =
            let rec matchSetIdentifierInternal tokens result =
                match tokens with
                | [] ->
                    invalidSyntax _position "Expected ']', got end of file"
                    //raise (SyntaxException ("Expected ']', got end of file", _position))
                | OfType RBrack _::xs ->
                    xs, Node.tag (SetIdentifierNode (List.rev result)) headPosition
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
            let rec matchDisjunct tokens startToken result =
                match tokens with
                | OfType Whitespace _::xs ->
                    matchDisjunct xs startToken result
                | OfType RParen _::xs ->
                    xs, Node.tag (DisjunctNode (List.rev result)) startToken.position
                | _ ->
                    let tokens, ruleSegment = matchRuleSegment tokens in
                    ruleSegment :: result
                    |> matchDisjunct tokens startToken

            /// <summary>
            /// Matches either an <see cref="OptionalNode" /> or a <see cref="DisjunctNode" />.
            /// </summary>
            /// <param name="tokens">The list of tokens.</param>
            let matchOptional_Disjunct tokens =
                let tokens, lparen = matchToken tokens LParen
                let rec matchOptional_DisjunctInteral tokens result =
                    match tokens with
                    | OfType Whitespace _::xs ->
                        matchOptional_DisjunctInteral xs result
                    | OfType RParen _::xs ->
                        xs, Node.tag (OptionalNode (List.rev result)) lparen.position
                    | OfType Pipe _::xs ->
                        matchDisjunct xs lparen [result]
                    | _ ->
                        let tokens, ruleSegment = matchRuleSegment tokens
                        (List.rev ruleSegment) @ result
                        |> matchOptional_DisjunctInteral tokens
                matchOptional_DisjunctInteral tokens []

            let rec inner tokens result =
                match tokens with
                | OfType Separator _::xs ->
                    inner xs result
                | OfType Id x::xs ->
                    inner xs (Node.tag (IdentifierNode x.value) x.position :: result)
                | OfType Utterance x::xs ->
                    inner xs (Node.tag (UtteranceNode x.value) x.position :: result)
                | OfType Placeholder x::xs ->
                    inner xs (Node.tag PlaceholderNode x.position :: result)
                | OfType Boundary x::xs ->
                    inner xs (Node.tag BoundaryNode x.position :: result)
                | OfType LBrack x::xs ->
                    let tokens, setIdentifier = matchSetIdentifier xs x.position
                    inner tokens (setIdentifier :: result)
                | OfType LParen x::xs ->
                    let tokens, optional = matchOptional_Disjunct tokens
                    inner tokens (optional :: result)
                | _ ->
                    tokens, List.rev result

            inner tokens []

        /// <summary>
        /// Matches either an <see cref="UtteranceNode" /> or a <see cref="TransformationNode" />.
        /// </summary>
        /// <param name="tokens">The list of tokens.</param>
        /// <param name="utterance">The utterance already matched.</param>
        let rec matchUtterance_Transformation tokens utterance =
            match tokens with
            | OfType Whitespace _::xs ->
                matchUtterance_Transformation xs utterance
            | OfType Gives _::xs ->
                let tokens, result = matchToken xs Utterance
                let targetNode = Node.tag (UtteranceNode utterance.value) utterance.position
                let resultNode = Node.tag (UtteranceNode result.value) result.position
                tokens, Node.tag (TransformationNode (targetNode, resultNode)) utterance.position
            | _ ->
                tokens, Node.tag (UtteranceNode utterance.value) utterance.position

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
                | OfType Whitespace _::xs
                | OfType Comment _::xs ->
                    matchMemberListInternal xs result
                | OfType Utterance x::xs ->
                    let tokens, utteranceOrTransformation = matchUtterance_Transformation xs x
                    matchMemberListInternal tokens (utteranceOrTransformation :: result)
                | OfType RBrace _::xs ->
                    xs, List.rev result
                | x::_ ->
                    unexpectedToken [Utterance] x
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
            tokens, Node.tag (RuleNode (target, replacement, environment)) headPosition

        /// <summary>
        /// Matches a rule when an identifier has already been matched.
        /// </summary>
        /// <param name="tokens">The list of tokens.</param>
        /// <param name="identifier">The identifier already matched.</param>
        let matchRuleStartingWithIdentifier tokens identifier =
            let tokens, ruleNode = matchRule tokens identifier.position
            match Node.untag ruleNode with
            | RuleNode (target, replacement, environment) ->
                tokens, Node.tag
                    (RuleNode (
                        Node.tag (IdentifierNode identifier.value) identifier.position :: target,
                        replacement,
                        environment))
                    identifier.position
            | _ ->
                raise (SyntaxException ("unexpected error", _position))

        /// <summary>
        /// Matches either a set or a rule.
        /// </summary>
        /// <param name="tokens">The list of tokens.</param>
        let rec matchSet_Rule tokens identifier =
            match tokens with
            | OfType Whitespace _::xs ->
                matchSet_Rule xs identifier
            | OfType LBrace _::xs ->
                // TODO: include members of other sets
                let tokens, members = matchMemberList xs
                tokens, Node.tag (SetDefinitionNode (identifier.value, members)) identifier.position
            | OfType Divider _::xs ->
                let tokens, ruleNode = matchRuleStartingWithIdentifier tokens identifier
                tokens, ruleNode
            | OfType Utterance _::xs ->
                let tokens, ruleNode = matchRuleStartingWithIdentifier xs identifier
                tokens, ruleNode
            | [] ->
                failwith "No more input"
            | x::_ ->
                raise (SyntaxException ("unexpected error", x.position))

        /// <summary>
        /// Matches a rule when a set identifier has already been matched.
        /// </summary>
        /// <param name="tokens">The list of tokens.</param>
        let matchRuleStartingWithSetIdentifier tokens headPosition =
            let tokens, setIdentifier = matchSetIdentifier tokens headPosition
            let tokens, ruleNode = matchRule tokens headPosition
            match Node.untag ruleNode with
            | RuleNode (target, replacement, environment) ->
                tokens, Node.tag (RuleNode (setIdentifier :: target, replacement, environment)) headPosition
            | _ ->
                raise (SyntaxException ("unexpected error", _position))

        /// <summary>
        /// Prepends a node list to the target segment of a RuleNode.
        /// </summary>
        /// <exception cref="System.ArgumentException">Thrown when the argument to <c>rule<c/>
        /// is not a <see cref="RuleNode" />.</exception>
        let prependToRule rule headPosition nodes =
            match Node.untag rule with
            | RuleNode (target, replacement, environment) ->
                Node.tag (RuleNode (nodes @ target, replacement, environment)) headPosition
            | _ ->
                invalidArg "rule" "Must be a RuleNode"

        /// <summary>
        /// Prepends a node list to the initial SetIdentifierNode of the target segment of a RuleNode.
        /// </summary>
        /// <exception cref="System.ArgumentException">Thrown when the argument to <c>rule<c/> is not a <see cref="RuleNode" />,
        /// or when the first element of the target segment is not a <see cref="SetIdentifierNode" />.</exception>
        let prependToRuleSetIdentifier rule headPosition nodes =
            match Node.untag rule with
            | RuleNode (target, _relacement, _environment) ->
                match Node.untag target.Head with
                | SetIdentifierNode identifiers ->
                    Node.tag
                        (RuleNode
                            (Node.tag (SetIdentifierNode (nodes @ identifiers)) headPosition :: target.Tail,
                            _relacement,
                            _environment))
                        headPosition
                | _ ->
                    invalidArg "rule" "First element of the target segment must be a SetIdentifierNode"
            | _ ->
                invalidArg "rule" "Must be a RuleNode"
                    
        /// <summary>
        /// Matches a feature definition.
        /// </summary>
        /// <param name="tokens">The list of tokens.</param>
        let matchFeature tokens headPosition identifier =
            let tokens, _ = matchToken tokens LBrace
            let tokens, memberList = matchMemberList tokens
            tokens, Node.tag (FeatureDefinitionNode (identifier.value, memberList)) headPosition


        /// <summary>
        /// Matches either a <see cref="FeatureDefinitionNode" />, a <see cref="SetIdentifierNode" />
        /// or a <see cref="RuleNode" />.
        /// </summary>
        /// <remarks>
        /// LBrack '[' is parsed just before entering this function.
        /// </remarks>
        /// <param name="tokens">The list of tokens.</param>
        let rec matchFeature_SetIdentifier_Rule tokens headPosition identifier =
            let optionalCata fSome fNone id =
                match id with
                | Some i -> fSome i
                | None -> fNone()

            match tokens with
            | [] ->
                raise (SyntaxException(sprintf "Expected feature identifier term, identifier or ']'; got end of file", _position))
            | OfType Plus _::_
            | OfType Minus _::_ ->
                let tokens, theSet = matchRuleStartingWithSetIdentifier tokens headPosition
                identifier
                |> optionalCata
                    // '[' Id [ '+' | '-' ] -> RuleNode (id :: target, replacement, environment)
                    (fun i -> tokens, prependToRuleSetIdentifier theSet headPosition [ Node.tag (IdentifierNode i.value) i.position ])
                    // '[' [ '+' | '-' ] -> RuleNode (...)
                    (fun _ -> tokens, theSet)
            | OfType RBrack x::xs ->
                identifier
                |> optionalCata
                    // '[' Id ']' -> FeatureDefinitionNode Id.name nodeList
                    (fun i -> matchFeature xs headPosition i)
                    // '[' ']' -> syntax error
                    (fun _ -> unexpectedToken [Id] x)
            | OfType Id x::xs ->
                identifier
                |> optionalCata
                    // '[' Id Id -> RuleNode ((Id :: (Id :: setIdentifier)) :: target.Tail, replacement, environment)
                    (fun i ->
                        let tokens, ruleNode = matchRule tokens x.position
                        tokens, prependToRule ruleNode headPosition
                            [ Node.tag (IdentifierNode i.value) i.position
                              Node.tag (IdentifierNode x.value) x.position
                            ])
                    // Store first identifier and see what we get next
                    (fun _ -> matchFeature_SetIdentifier_Rule xs headPosition (Some x))
            | x::_ ->
                unexpectedToken [ Plus; Minus; Id ] x

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
                | OfType Whitespace _::xs ->
                    nextInternal xs
                | OfType Comment x::xs ->
                    NextResult.OK (xs, Node.tag (CommentNode x.value) x.position)
                | OfType Utterance x::xs ->
                    NextResult.OK (matchRule tokens x.position)
                | OfType Id x::xs ->
                    NextResult.OK (matchSet_Rule xs x)
                | OfType LBrack x::xs ->
                    NextResult.OK (matchFeature_SetIdentifier_Rule xs x.position None)
                | x::_ ->
                    NextResult.SyntaxError (sprintf "Unexpected token '%s'" x.value, x.position)
            with
                | :? SyntaxException as ex ->
                    NextResult.SyntaxError (ex.Message, ex.Position)
   
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
        let rec parseInternal tokens result =
            match tokens with
            | [] ->
                List.rev result
            | _ ->
                match next tokens with
                | NextResult.OK (nextTokens, node) ->
                    parseInternal nextTokens (node :: result)
                | NextResult.SyntaxError (message, position) ->
                    raise (SyntaxException (message, position))
        parseInternal tokens []
