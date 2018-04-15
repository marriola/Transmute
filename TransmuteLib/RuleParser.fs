namespace TransmuteLib

open StateMachine
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
    | IdentifierNode of string
    | SetIdentifierNode of Node list
    | SetIdentifierTermNode of name:string
    | FeatureIdentifierTermNode of isPresent:bool * name:string
    | CompoundSetNode of Node list
    | UtteranceNode of string
    | PlaceholderNode
    | BoundaryNode
    | RuleNode of target:Node list * result:Node list * environment:Node list
    | SetDefinitionNode of name:string * members:Node list
    | FeatureDefinitionNode of name:string * members:Node list
    | OptionalNode of Node list
    | DisjunctNode of Node list
    | TransformationNode of target:Node * result:Node

type RuleParserResult =
    | OK of Node list
    | SyntaxError of string * (int * int)

type NextResult =
    | OK of Token list * Node
    | SyntaxError of string * (int * int)

module RuleParser =
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
        let rec matchSet (tokens: Token list) (targetTypes: TokenType list) =
            let tokenType = tokens.Head.tokenType
            if tokenType = Whitespace then
                matchSet tokens.Tail targetTypes
            else if List.contains tokenType targetTypes then
                tokens.Tail, tokens.Head
            else
                raise (ExpectedSetException (targetTypes, tokens.Head))

        let matchFeatureIdentifierTerm tokens =
            let tokens, presence = matchSet tokens [ Plus; Minus ]
            let tokens, identifier = matchToken tokens Id
            tokens, FeatureIdentifierTermNode (presence.tokenType = Plus, identifier.value)

        let matchSetIdentifierTerm tokens =
            let tokens, identifier = matchToken tokens Id
            tokens, SetIdentifierTermNode identifier.value

        let matchSetOrFeatureIdentifierTerm (tokens: Token list) =
            match tokens.Head.tokenType with
            | Id ->
                matchSetIdentifierTerm tokens
            | Plus | Minus ->
                matchFeatureIdentifierTerm tokens
            | _ ->
                raise (ExpectedSetException ([ Id; Plus; Minus ], tokens.Head))

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

        let rec matchUtterance_Transformation (tokens: Token list) utterance =
            match tokens.Head.tokenType with
            | Whitespace ->
                matchUtterance_Transformation tokens.Tail utterance
            | Gives ->
                let tokens, result = matchToken tokens.Tail Utterance
                tokens, TransformationNode (utterance, UtteranceNode result.value)
            | _ ->
                tokens, utterance

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

        let matchRule tokens =
            let tokens, target = matchRuleSegment tokens
            let tokens, _ = matchToken tokens Divider
            let tokens, replacement = matchRuleSegment tokens
            let tokens, _ = matchToken tokens Divider
            let tokens, environment = matchRuleSegment tokens
            tokens, RuleNode (target, replacement, environment)

        let matchRuleStartingWithIdentifier tokens (identifier: Token) =
            let tokens, ruleNode = matchRule tokens
            match ruleNode with
            | RuleNode (target, replacement, environment) ->
                tokens, RuleNode (IdentifierNode identifier.value :: target, replacement, environment)
            | _ -> raise (SyntaxException ("unexpected error", _position))

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

        let matchRuleStartingWithSetIdentifier tokens =
            let tokens, setIdentifier = matchSetIdentifier tokens
            let tokens, ruleNode = matchRule tokens
            match ruleNode with
            | RuleNode (target, replacement, environment) ->
                tokens, RuleNode (setIdentifier :: target, replacement, environment)
            | _ ->
                raise (SyntaxException ("unexpected error", _position))
                    
        let matchFeature tokens =
            let tokens, identifier = matchToken tokens Id
            let tokens, _ = matchToken tokens RBrack
            let tokens, _ = matchToken tokens LBrace
            let tokens, memberList = matchMemberList tokens
            tokens, FeatureDefinitionNode (identifier.value, memberList)

        let matchFeature_SetIdentifier_Rule (tokens: Token list) =
            match tokens.Head.tokenType with
            | Plus | Minus ->
                matchRuleStartingWithSetIdentifier tokens
            | Id ->
                matchFeature tokens
            | _ ->
                raise (ExpectedSetException ([ Plus; Minus; Id ], tokens.Head))

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
   
        let result = (nextInternal tokens)

        match result with
        | OK (nextTokens, node) ->
            if [] <> nextTokens then
                _position <- nextTokens.Head.position
            result
        | _ -> result

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
