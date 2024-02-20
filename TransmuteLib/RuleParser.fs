// Project:     TransmuteLib
// Module:      RuleParser
// Description: Rule parser
// Copyright:   (c) 2023 Matt Arriola
// License:     MIT

namespace TransmuteLib

open System.IO
open TransmuteLib.ExceptionHelpers
open TransmuteLib.Lexer
open TransmuteLib.Position
open TransmuteLib.Token
open TransmuteLib.Utils.Operators

module RuleParser =
    /// <summary>
    /// Parses the next node in the list of tokens.
    /// </summary>
    /// <param name="tokens">The list of tokens.</param>
    let private next tokens =
        let mutable _position = Offset 0, Line 1, Column 1

        /// <summary>
        /// Matches a token to a specific type.
        /// <summary>
        /// <exception cref="SyntaxException">Thrown when the token at the head of the list does not match the given type.</exception>
        let rec matchToken tokens tokenType =
            match tokens with
            | [] ->
                invalidArg "tokens" "Must not be empty"
            | OfType Whitespace _::xs ->
                matchToken xs tokenType
            | { tokenType = t } as x::xs when tokenType = t ->
                xs, x
            | x::_ ->
                unexpectedToken [tokenType] x

        let rec tryMatchToken tokens tokenType =
            match tokens with
            | OfType Whitespace _::xs ->
                tryMatchToken xs tokenType
            | { tokenType = t } as x::xs when tokenType = t ->
                xs, Some t
            | _ ->
                tokens, None

        let rec tryMatchTokenOnSameLine tokens tokenType =
            match tokens with
            | OfType Whitespace ws::xs ->
                if ws.value.Contains "\n" then
                    xs, None
                else
                    tryMatchToken xs tokenType
            | { tokenType = t } as x::xs when tokenType = t ->
                xs, Some t
            | _ ->
                tokens, None

        /// Matches a token to one of a list of token types, automatically skipping over any whitespace.
        /// </summary>
        /// <param name="tokens">The list of tokens.</param>
        /// <param name="tokenTypes">The list of valid token types.</param>
        let rec matchOneOf tokens tokenTypes =
            match tokens with
            | [] ->
                invalidArg "tokens" "Must not be empty"
            | OfType Whitespace _::xs ->
                matchOneOf xs tokenTypes
            | { tokenType = t } as x::xs when List.contains t tokenTypes ->
                xs, x
            | x::_ ->
                unexpectedToken tokenTypes x

        let matchUtteranceIdentifierNode (tokens: Token list) isPresent =
            let rec inner last tokens out =
                match tokens with
                | [] ->
                    failwith $"Expected segment or '/', got end of file at {last.position}"

                | { tokenType = Whitespace } as token :: rest ->
                    if token.value.Contains "\n" then
                        failwith $"Expected segment or '/', got end of line at {token.position}"
                    else
                        inner token rest out

                | { tokenType = Divider } :: rest ->
                    rest, SegmentIdentifierNode (isPresent, List.rev out)

                | { tokenType = Utterance } as token :: rest ->
                    inner token rest (token.value :: out)

                | token :: _ ->
                    failwith $"Expected segment or '/', got {token} at {token.position}"

            inner tokens[0] tokens[1..] []

        /// <summary>
        /// Matches a <see cref="FeatureIdentifierNode" />.
        /// </summary>
        /// <param name="tokens">The list of tokens.</param>
        let matchFeatureIdentifierTerm tokens =
            let tokens, presence = matchOneOf tokens [ Plus; Minus ]
            let rest, term = matchOneOf tokens [ Id; Divider ]
            let isPresent = presence.tokenType = Plus
            let tokens, node =
                match term with
                | { tokenType = Id } ->
                    rest, FeatureIdentifierNode (isPresent, term.value)
                | { tokenType = Divider } ->
                    matchUtteranceIdentifierNode tokens isPresent
            tokens, Node.tag node presence.position

        /// <summary>
        /// Matches a <see cref="TermIdentifierNode" />.
        /// </summary>
        /// <param name="tokens">The list of tokens.</param>
        let matchSetIdentifierTerm tokens =
            let tokens, identifier = matchToken tokens Id
            tokens, Node.tag (TermIdentifierNode identifier.value) identifier.position

        /// <summary>
        /// Matches either a <see cref="TermIdentifierNode" /> or a <see cref="FeatureIdentifierNode" />.
        /// </summary>
        /// <param name="tokens">The list of tokens.</param>
        let matchSetOrFeatureIdentifierTerm tokens =
            match tokens with
            | [] ->
                invalidSyntax "Expected '+', '-' or an identifier, got end of file" _position
            | OfType Id _::_ ->
                matchSetIdentifierTerm tokens
            | OfType Plus _::_
            | OfType Minus _::_ ->
                matchFeatureIdentifierTerm tokens
            | x::_ ->
                unexpectedToken [ Id; Plus; Minus ] x

        /// <summary>
        /// Matches a <see cref="CompoundSetIdentifierNode" />.
        /// </summary>
        /// <param name="tokens">The list of tokens.</param>
        let matchSetIdentifier tokens headPosition =
            let rec matchSetIdentifierInternal tokens out =
                match tokens with
                | [] ->
                    invalidSyntax "Expected '+', '-' or an identifier, got end of file" _position
                | NewlineWhitespace _::_ ->
                    invalidSyntax "Expected '+', '-' or an identifier; got end of line" _position
                | NonNewlineWhitespace _::xs ->
                    matchSetIdentifierInternal xs out
                | OfType RBrack _::xs ->
                    xs, Node.tag (CompoundSetIdentifierNode (List.rev out)) headPosition
                | _ ->
                    let tokens, term = matchSetOrFeatureIdentifierTerm tokens
                    matchSetIdentifierInternal tokens (term :: out)
            matchSetIdentifierInternal tokens []

        /// <summary>
        /// Matches a rule section, i.e. a list of <see cref="SetIdentifierNode" />, <see cref="UtteranceNode" />,
        /// <see cref="PlaceholderNode" />, <see cref="BoundaryNode" />, <see cref="CompoundSetIdentifierNode" />,
        /// <see cref="OptionalNode" /> or <see cref="DisjunctNode" />.
        /// </summary>
        /// <param name="tokens">The list of tokens.</param>
        let rec matchRuleSection tokens =
            /// <summary>
            /// Matches a <see cref="DisjunctNode" />.
            /// </summary>
            /// <param name="tokens">The list of tokens.</param>
            /// <param name="out">The contents of the node.</param>
            let rec matchDisjunct tokens startToken out =
                match tokens with
                | OfType Empty _::xs ->
                    matchDisjunct xs startToken out
                | NewlineWhitespace _::_ ->
                    invalidSyntax "Expected '|', ')', an utterance, or an identifier; got end of line" _position
                | OfType Whitespace _::xs ->
                    matchDisjunct xs startToken out
                | OfType RParen _::xs ->
                    xs, Node.tag (DisjunctNode (List.rev out)) startToken.position
                | OfType Pipe _::xs ->
                    matchDisjunct xs startToken out
                | _ ->
                    let tokens, ruleSection = matchRuleSection tokens
                    ruleSection :: out
                    |> matchDisjunct tokens startToken

            /// <summary>
            /// Matches either an <see cref="OptionalNode" /> or a <see cref="DisjunctNode" />.
            /// </summary>
            /// <param name="tokens">The list of tokens.</param>
            let matchOptional_Disjunct tokens =
                let tokens, lparen = matchToken tokens LParen
                let rec matchOptional_DisjunctInteral tokens out =
                    match tokens with
                    | NewlineWhitespace _::_ ->
                        invalidSyntax "Expected '|', ')', an utterance, or an identifier; got end of line" _position
                    | OfType Whitespace _::xs ->
                        matchOptional_DisjunctInteral xs out
                    | OfType RParen _::xs ->
                        xs, Node.tag (OptionalNode (List.rev out)) lparen.position
                    | OfType Pipe _::xs ->
                        matchDisjunct xs lparen [out]
                    | _ ->
                        let tokens, ruleSection = matchRuleSection tokens
                        matchOptional_DisjunctInteral tokens (ruleSection @ out)
                matchOptional_DisjunctInteral tokens []

            let rec inner tokens out =
                match tokens with
                | OfType Empty _ :: xs
                | OfType Separator _ :: xs ->
                    inner xs out
                | NonNewlineWhitespace _ :: xs ->
                    inner xs out
                | OfType Id x :: xs ->
                    inner xs (Node.tag (SetIdentifierNode x.value) x.position :: out)
                | OfType Utterance x :: xs ->
                    inner xs (Node.tag (UtteranceNode x.value) x.position :: out)
                | OfType Placeholder x :: xs ->
                    inner xs (Node.tag PlaceholderNode x.position :: out)
                | OfType WordBoundary x :: xs ->
                    inner xs (Node.tag WordBoundaryNode x.position :: out)
                | OfType LBrack x :: xs ->
                    let tokens, setIdentifier = matchSetIdentifier xs x.position
                    inner tokens (setIdentifier :: out)
                | OfType LParen _ :: _ ->
                    let tokens, optional = matchOptional_Disjunct tokens
                    inner tokens (optional :: out)
                | _ ->
                    tokens, List.rev out

            inner tokens []

        /// <summary>
        /// Either matches an arrow and an utterance and produces a <see cref="TransformationNode" />, or produces an <see cref="UtteranceNode" />
        /// with the utterance already parsed.
        /// </summary>
        /// <param name="tokens">The list of tokens.</param>
        /// <param name="utterance">The utterance already matched.</param>
        let rec matchUtterance_Transformation tokens utterance =
            match tokens with
            | OfType Whitespace _::xs ->
                matchUtterance_Transformation xs utterance
            | OfType Arrow _::xs ->
                let tokens, token = matchToken xs Utterance
                let inputNode = Node.tag (UtteranceNode utterance.value) utterance.position
                let outputNode = Node.tag (UtteranceNode token.value) token.position
                tokens, Node.tag (TransformationNode (inputNode, outputNode)) utterance.position
            | _ ->
                tokens, Node.tag (UtteranceNode utterance.value) utterance.position

        /// <summary>
        /// Matches a list of set/feature members. These may be of type <see cref="UtteranceNode" /> or
        /// <see cref="TransformationNode" />.
        /// </summary>
        /// <param name="tokens">The list of tokens.</param>
        let matchMemberList closeToken tokens =
            let rec matchMemberListInternal tokens out =
                match tokens with
                | [] ->
                    invalidSyntax "Expected member list, got end of file" _position
                | OfType Whitespace _::xs
                | OfType Comment _::xs ->
                    matchMemberListInternal xs out
                | OfType Utterance x::xs ->
                    let tokens, utteranceOrTransformation = matchUtterance_Transformation xs x
                    let tokens, _ = tryMatchToken tokens Comma
                    matchMemberListInternal tokens (utteranceOrTransformation :: out)
                | OfType Id x::xs ->
                    let id = Node.tag (SetIdentifierNode x.value) x.position
                    let tokens, _ = tryMatchToken xs Comma
                    matchMemberListInternal tokens (id :: out)
                | x::xs when x.tokenType = closeToken ->
                    xs, List.rev out
                | OfType LBrack x::xs ->
                    let tokens, id = matchSetIdentifier xs x.position
                    matchMemberListInternal tokens (id :: out)
                | x::_ ->
                    unexpectedToken [Utterance] x
            matchMemberListInternal tokens []

        /// <summary>
        /// Matches a rule.
        /// </summary>
        /// <param name="tokens">The list of tokens.</param>
        let matchRule tokens headPosition =
            let _, Line startLine, _ = headPosition
            let tokens, input = matchRuleSection tokens
            let tokens, _ = matchOneOf tokens [ Arrow; Divider ]
            let tokens, output = matchRuleSection tokens
            let tokens, divider = tryMatchTokenOnSameLine tokens Divider
            let tokens, environment =
                match divider with
                | Some _ ->
                    let tokens, environment = matchRuleSection tokens
                    tokens, environment
                | None ->
                    tokens, [PlaceholderNode]
            tokens, Node.tag (RuleNode (startLine, input, output, environment)) headPosition


        /// <summary>
        /// Matches a rule when an identifier has already been matched.
        /// </summary>
        /// <param name="tokens">The list of tokens.</param>
        /// <param name="identifier">The identifier already matched.</param>
        let matchRuleStartingWithIdentifier tokens identifier =
            let tokens, ruleNode = matchRule tokens identifier.position
            match Node.untag ruleNode with
            | RuleNode (lineNumber, input, output, environment) ->
                tokens, Node.tag
                    (RuleNode (
                        lineNumber,
                        Node.tag (SetIdentifierNode identifier.value) identifier.position :: input,
                        output,
                        environment))
                    identifier.position
            | _ ->
                invalidSyntax "Expected a rule" _position

        let matchAssignment tokens =
            let tokens, identifier = matchToken tokens Id
            let tokens, _ = matchToken tokens Equals
            let tokens, nodes = matchRuleSection tokens
            tokens, identifier.value, nodes

        let matchSyllableDefinition tokens =
            let startToken = List.head tokens

            let rec inner tokens out =
                match tokens with
                | []
                | OfType RParen _ :: _->
                    tokens, List.rev out
                | OfType Comment _ :: xs
                | OfType Whitespace _ :: xs ->
                    inner xs out
                | OfType Id _ :: _ ->
                    let xs, name, nodes = matchAssignment tokens
                    inner xs ((name, nodes) :: out)
                | token::_ ->
                    invalidSyntax $"Expected 'Onset', 'Nucleus' or 'Coda', got {token}" token.position

            let tokens, _ = matchToken tokens LParen
            let tokens, parts = inner tokens []
            let tokens, _ = matchToken tokens RParen

            let parts =
                parts
                |> List.groupBy fst
                |> List.map (fun (key, parts) ->
                    match parts with
                    | [] -> key, []
                    | [_, nodes] -> key, nodes
                    | _ -> 
                        let disjunctNode =
                            parts
                            |> List.map (fun (_, nodes) -> nodes)
                            |> DisjunctNode
                        key, [ disjunctNode ])

            let findSegmentOrEmpty name parts =
                parts
                |> List.tryFind (fst >> (=) name)
                |> Option.map snd
                |> Option.defaultValue []

            let onset = findSegmentOrEmpty "Onset" parts
            let nucleus = findSegmentOrEmpty "Nucleus" parts
            let coda = findSegmentOrEmpty "Coda" parts

            if onset = [] && nucleus = [] && coda = [] then
                invalidSyntax "All segments are missing or empty in syllable definition" startToken.position

            tokens, SyllableDefinitionNode (onset, nucleus, coda)

        let matchSyllableDefinitions tokens =
            let rec inner matchedOr out tokens =
                match tokens with
                | [] ->
                    tokens, List.rev out

                | OfType Whitespace ws :: _ when ws.value.Contains "\n" ->
                    tokens, List.rev out

                | OfType Whitespace _ :: xs ->
                    inner matchedOr out xs

                | OfType Utterance u :: xs when u.value.Trim().ToLower() = "or" ->
                    inner true out xs

                | OfType LParen _ :: _ ->
                    let xs, syllableDefinition = matchSyllableDefinition tokens
                    inner false (syllableDefinition :: out) xs

            inner false [] tokens

        /// <summary>
        /// Matches either a set or a rule.
        /// </summary>
        /// <param name="tokens">The list of tokens.</param>
        let rec matchSet_Rule tokens identifier =
            match tokens with
            | OfType Separator _::xs
            | OfType Whitespace _::xs ->
                matchSet_Rule xs identifier
            | OfType Equals _::xs when identifier.value = "Syllable" ->
                let _, Line lineNumber, _ = xs.[0].position
                let xs, syllableDefinitions = matchSyllableDefinitions xs
                xs, (SyllableDefinitionListNode (lineNumber, syllableDefinitions))
            | OfType Equals _ :: xs ->
                let xs, openToken = matchOneOf xs [ LParen; LBrace ]
                let closeToken = if openToken.tokenType = LBrace then RBrace else RParen
                let tokens, members = matchMemberList closeToken xs
                tokens, Node.tag (SetDefinitionNode (identifier.value, members)) identifier.position
            | OfType Arrow _::xs
            | OfType Divider _::xs ->
                let tokens, ruleNode = matchRuleStartingWithIdentifier tokens identifier
                tokens, ruleNode
            | OfType Utterance _::xs ->
                let tokens, ruleNode = matchRuleStartingWithIdentifier tokens identifier
                tokens, ruleNode
            | [] ->
                failwith "No more input"
            | x::_ ->
                unexpectedToken [ Equals; LBrace; Empty; Divider; Utterance ] x

        /// <summary>
        /// Matches a rule when a set identifier has already been matched.
        /// </summary>
        /// <param name="tokens">The list of tokens.</param>
        let matchRuleStartingWithSetIdentifier tokens headPosition =
            let tokens, setIdentifier = matchSetIdentifier tokens headPosition
            let tokens, ruleNode = matchRule tokens headPosition
            match Node.untag ruleNode with
            | RuleNode (lineNumber, input, output, environment) ->
                tokens, Node.tag (RuleNode (lineNumber, setIdentifier :: input, output, environment)) headPosition
            | _ ->
                invalidSyntax "Expected a rule" _position

        /// <summary>
        /// Prepends a node list to the input section of a RuleNode.
        /// </summary>
        /// <exception cref="System.ArgumentException">Thrown when the argument to <c>rule<c/>
        /// is not a <see cref="RuleNode" />.</exception>
        let prependToRule rule headPosition nodes =
            match Node.untag rule with
            | RuleNode (lineNumber, input, output, environment) ->
                Node.tag (RuleNode (lineNumber, nodes @ input, output, environment)) headPosition
            | _ ->
                invalidArg "rule" "Must be a RuleNode"

        /// <summary>
        /// Prepends a node list to the initial CompoundSetIdentifierNode of the input section of a RuleNode.
        /// </summary>
        /// <exception cref="System.ArgumentException">Thrown when the argument to <c>rule<c/> is not a <see cref="RuleNode" />,
        /// or when the first element of the input section is not a <see cref="CompoundSetIdentifierNode" />.</exception>
        let prependToRuleSetIdentifier rule headPosition nodes =
            match Node.untag rule with
            | RuleNode (lineNumber, input, output, environment) ->
                match Node.untag input.Head with
                | CompoundSetIdentifierNode identifiers ->
                    Node.tag
                        (RuleNode
                            (lineNumber,
                            Node.tag (CompoundSetIdentifierNode (nodes @ identifiers)) headPosition :: input.Tail,
                            output,
                            environment))
                        headPosition
                | _ ->
                    invalidArg "rule" "First element of the input section must be a CompoundSetIdentifierNode"
            | _ ->
                invalidArg "rule" "Must be a RuleNode"
                    
        /// <summary>
        /// Matches a feature definition.
        /// </summary>
        /// <param name="tokens">The list of tokens.</param>
        let matchFeature tokens headPosition identifier =
            let tokens, _ = matchToken tokens Equals
            let tokens, openToken = matchOneOf tokens [ LBrace; LParen ]
            let closeToken = if openToken.tokenType = LBrace then RBrace else RParen
            let tokens, memberList = matchMemberList closeToken tokens
            tokens, Node.tag (FeatureDefinitionNode (identifier.value, memberList)) headPosition


        /// <summary>
        /// Matches either a <see cref="FeatureDefinitionNode" />, a <see cref="CompoundSetIdentifierNode" />
        /// or a <see cref="RuleNode" />.
        /// </summary>
        /// <remarks>
        /// LBrack '[' is parsed just before entering this function.
        /// </remarks>
        /// <param name="tokens">The list of tokens.</param>
        let rec matchFeature_SetIdentifier_Rule tokens headPosition identifier =
            match tokens with
            | [] ->
                invalidSyntax "Expected feature identifier term, identifier or ']'; got end of file" _position
            | NewlineWhitespace _::_ ->
                invalidSyntax "Expected '+', '-', ']' or an identifier; got end of line" _position
            | NonNewlineWhitespace _::xs ->
                matchFeature_SetIdentifier_Rule xs headPosition identifier
            | OfType Plus _::_
            | OfType Minus _::_ ->
                let tokens, theSet = matchRuleStartingWithSetIdentifier tokens headPosition
                identifier
                // '[' Id [ '+' | '-' ] -> RuleNode (id :: input, output, environment)
                |> Option.map (fun i -> tokens, prependToRuleSetIdentifier theSet headPosition [ Node.tag (SetIdentifierNode i.value) i.position ])
                // '[' [ '+' | '-' ] -> RuleNode (...)
                |> Option.defaultWith (fun _ -> tokens, theSet)
            | OfType RBrack x::xs ->
                identifier
                // '[' Id ']' -> FeatureDefinitionNode Id.name nodeList
                |> Option.map (fun i -> matchFeature xs headPosition i)
                // '[' ']' -> syntax error
                |> Option.defaultWith (fun _ -> unexpectedToken [Id] x)
            | OfType Id idToken::xs ->
                identifier
                // '[' Id Id -> RuleNode ((Id :: (Id :: setIdentifier)) :: input.Tail, output, environment)
                |> Option.map (fun i ->
                    let tokens, ruleNode = matchRule tokens idToken.position
                    tokens, prependToRule ruleNode headPosition
                        [ Node.tag (SetIdentifierNode i.value) i.position
                          Node.tag (SetIdentifierNode idToken.value) idToken.position
                        ])
                // Store first identifier and see what we get next
                |> Option.defaultWith (fun _ -> matchFeature_SetIdentifier_Rule xs headPosition (Some idToken))
            | x::_ ->
                unexpectedToken [ Plus; Minus; Id ] x

        let matchSyllableRule tokens =
            let tokens, _ = matchToken tokens Equals
            matchRuleSection tokens

        /// <summary>
        /// Determines which rule to match to the available tokens.
        /// </summary>
        /// <param name="tokens">The list of tokens.</param>
        /// <exception cref="SyntaxException">No rule matches the available tokens.</exception>
        let rec nextInternal tokens =
            try
                _position <-
                    match tokens with
                    | [] -> _position
                    | x:: _ -> x.position

                match tokens with
                | [] ->
                    Result.Error (syntaxErrorMessage "End of file" _position)
                | OfType Whitespace _::xs ->
                    nextInternal xs
                | OfType Comment x::xs ->
                    Ok (xs, Node.tag (CommentNode x.value) x.position)
                | OfType Empty x::_
                | OfType Divider x::_
                | OfType Utterance x::_
                | OfType LParen x::_ ->
                    Ok (matchRule tokens x.position)
                | OfType Id x::xs ->
                    Ok (matchSet_Rule xs x)
                | OfType LBrack x::xs ->
                    Ok (matchFeature_SetIdentifier_Rule xs x.position None)
                | x::_ ->
                    Result.Error (syntaxErrorMessage (sprintf "Unexpected token '%s'" x.value)  _position)
            with
                Exceptions.SyntaxError (message, offset, row, col) ->
                    Result.Error (syntaxErrorMessage message (offset, row, col))
   
        nextInternal tokens

    /// <summary>
    /// Parses a list of tokens to a list of nodes.
    /// </summary>
    /// <param name="tokens">The list of tokens to parse.</param>
    let private parse tokens =
        let rec parseInternal tokens out =
            match tokens with
            | [] ->
                Ok (List.rev out)
            | _ ->
                match next tokens with
                | Ok (nextTokens, node) ->
                    parseInternal nextTokens (node :: out)
                | Result.Error message ->
                    Result.Error message
        parseInternal tokens []

    let parseRules inputFormat content =
        let tokens =
            match lex inputFormat content with
            | FileError msg ->
                Result.Error (sprintf "%s" msg)
            | SyntaxError (msg, Offset offset, Line row, Column col) ->
                Result.Error (sprintf "Syntax error at row %d column %d (offset %d): %s" row col offset msg)
            | OK tokens ->
                Result.Ok tokens

        let nodes =
            tokens
            |> Result.bind parse
            |> Result.bind SyntaxAnalyzer.validate
            |> Result.bind (Node.untagAll >> Ok)

        let syllableDefinitions =
            nodes
            |> Result.map (List.collect (function
                | SyllableDefinitionListNode (line, definitions) ->
                    List.map (fun d -> line, d) definitions
                | _ ->
                    []))

        //let syllableDefinitions =
        //    nodes
        //    |> Result.map (List.choose (function
        //        | SyllableDefinitionListNode (line, definitions) ->
        //            Some (line, definitions)
        //        | _ ->
        //            None))

        let features = Result.map Node.getFeatures nodes
        let sets = Result.map Node.getSets nodes

        let rules =
            nodes
            |> Result.bind (Ok << List.choose (function
                | RuleNode _ as x -> Some x
                | _ -> None))

        match syllableDefinitions, features, sets, rules with
        | _, _, _, (Result.Error msg)
        | _, _, (Result.Error msg), _
        | _, (Result.Error msg), _, _
        | (Result.Error msg), _, _, _ ->
            Result.Error msg
        | Ok (syllableDefinition), (Ok features), (Ok sets), (Ok rules) ->
            // Resolve references to other sets and features
            let resolvedFeatures =
                features
                |> Map.toList
                |> List.map (fun (name, node) -> name, Node.resolveReferences features sets node)
                |> Map.ofList

            let resolvedSets =
                sets
                |> Map.toList
                |> List.map (fun (name, node) -> name, Node.resolveReferences features sets node)
                |> Map.ofList

            Ok (syllableDefinition, resolvedFeatures, resolvedSets, rules)

#if !FABLE_COMPILER
    let parseRulesStreamReader inputFormat (reader: StreamReader) =
        let content = reader.ReadToEnd()
        parseRules inputFormat content

    let parseRulesTextReader inputFormat (reader: TextReader) =
        let content = reader.ReadToEnd()
        parseRules inputFormat content

    let parseRulesStream inputFormat (stream: Stream) =
        use reader = new StreamReader(stream)
        let content = reader.ReadToEnd()
        parseRules inputFormat content

    let parseRulesFile inputFormat (path: string) =
        use reader = new StreamReader(path, true)
        parseRulesStreamReader inputFormat reader
#endif
