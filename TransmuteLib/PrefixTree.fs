namespace TransmuteLib

open System.Collections.Generic

type PrefixTree =
    | PrefixNode of prefix:string * value:char * children:Dictionary<char, PrefixTree>

module PrefixTree =
    open System
    open TransmuteLib
    open TransmuteLib.Node
    open TransmuteLib.Exceptions

    // maketree [ "k"; "kw"; "g"; "gw"; "p"; "b"; "t"; "d" ]

    //                Ø
    //                |
    //    +--------+----+--+--+--+
    //    |        |    |  |  |  |
    //    k        g    p  b  t  d
    // w / \ Ø  w / \ Ø
    //  kw  k    gw  g

    let NUL = '\u2400'
    let START_MATCH = '␂'
    let END_MATCH = '␃'

    /// <summary>
    /// Creates a prefix tree from a set of utterances.
    /// </summary>
    /// <param name="set">The set of utterances.</param>
    let makeTree set =
        let rec inner root subtree accumulator utterance =
            if utterance = "" then
                root
            else
                let headSymbol = utterance.[0]
                let rest = utterance.[1..]
                let next =
                    if headSymbol = NUL then
                        accumulator
                    else
                        accumulator + (string headSymbol)
                match subtree with
                | PrefixNode (_, _, children) ->
                    if children.ContainsKey headSymbol then
                        inner root children.[headSymbol] next rest
                    else
                        let nextSubtree = PrefixNode(next, headSymbol, new Dictionary<char, PrefixTree>())
                        children.[headSymbol] <- inner nextSubtree nextSubtree next rest
                        root
        let root = PrefixNode ("", NUL, new Dictionary<char, PrefixTree>())
        let rec loopOverSet (set: string list) =
            if set.IsEmpty then
                root
            else
                let suffixedUtterance = set.Head + (string NUL)
                ignore (inner root root "" suffixedUtterance)
                loopOverSet set.Tail
        loopOverSet set

    /// <summary>
    /// Creates a syntax error from a tagged node.
    /// </summary>
    /// <param name="node"></param>
    /// <param name="format"></param>
    let private makeSyntaxError node format =
        let position, innerNode = Node.untagWithMetadata node
        raise (SyntaxException (format position innerNode, position))

    /// <summary>
    /// Creates a symbol undefined error message.
    /// </summary>
    /// <param name="kind">The kind of object that is undefined.</param>
    /// <param name="name">The name of the undefined object.</param>
    /// <param name="position">The position of the token that named the undefined object.</param>
    /// <param name="node">The node that named the undefined object.</param>
    let private undefinedSetOrFeature kind name position node =
        sprintf "'%s' '%s' not defined" kind name

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
                undefinedSetOrFeature kind name
                    |> makeSyntaxError node
                    |> raise

    /// <summary>
    /// Computes the intersection of the sets and features named in the SetIdentifierNode.
    /// </summary>
    /// <param name="sets">The available sets.</param>
    /// <param name="features">The available features.</param>
    /// <param name="setIdentifier"></param>
    let internal setIntersection (sets: IDictionary<string, Node>) (features: IDictionary<string, Node>) setIdentifier =
        let rec inner (terms: Node list) (result: Set<string>) =
            match terms with
            | [] ->
                result
            | x::xs ->
                let setMembers =
                    match Node.untag x with
                    | IdentifierNode name ->
                        tryFindSetOrFeature (fun _ -> getSetMembers sets.[name]) "Set" x name |> set
                    | FeatureIdentifierTermNode (isPresent, name) ->
                        tryFindSetOrFeature (fun _ -> getFeatureMembers features.[name] isPresent) "Feature" x name |> set
                    | _ ->
                        let position, node = Node.untagWithMetadata x
                        raise (SyntaxException (sprintf "Unexpected token '%s'" (string node), position))
                let nextSet =
                    if result.IsEmpty then
                        setMembers
                    else
                        let differenceRight = result - setMembers
                        let differenceLeft = setMembers - result
                        result - differenceLeft - differenceRight
                inner xs nextSet
        match Node.untag setIdentifier with
        | SetIdentifierNode terms ->
            inner terms Set.empty |> List.ofSeq
        | _ ->
            raise (ArgumentException ("Must be a SetIdentifierNode", "setIdentifier"))

type PrefixTree with
    /// <summary>
    /// Creates a prefix tree from the members of a feature
    /// </summary>
    /// <param name="feature"></param>
    /// <param name="isPresent"></param>
    static member fromFeature feature isPresent =
        PrefixTree.makeTree (Node.getFeatureMembers feature isPresent)

    /// <summary>
    /// Creates a prefix tree from the members of a set.
    /// </summary>
    /// <param name="set"></param>
    static member fromSet set =
        PrefixTree.makeTree (Node.getSetMembers set)

    /// <summary>
    /// Creates a prefix tree from the intersection of a list of sets and features.
    /// </summary>
    /// <param name="sets">The available sets.</param>
    /// <param name="features">The available features.</param>
    /// <param name="setIdentifier">The SetIdentifierNode listing the sets and features to intersect.</param>
    static member fromSetIntersection sets features setIdentifier =
        PrefixTree.setIntersection sets features setIdentifier |> PrefixTree.makeTree
