namespace TransmuteLib

open System.Collections.Generic

type PrefixTree =
    | PrefixNode of prefix:string * value:char * children:IDictionary<char, PrefixTree>
    | Leaf of prefix:string * value:char

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

    let private isEmpty = String.length >> (=) 0

    /// <summary>
    /// Creates a prefix tree from a set of utterances.
    /// </summary>
    /// <param name="set">The set of utterances.</param>
    let makeTree set =
        let rec inner set acc =
            let final =
                if List.exists isEmpty set
                    then [ NUL, Leaf (acc, NUL) ]
                    else []
            let followSet =
                set
                |> List.where (not << isEmpty)
                |> List.groupBy Seq.head
                |> List.map (fun (prefixChar, subset) ->
                    let subset = subset |> List.map (fun s -> s.[1..])
                    let nextAcc = acc + (string prefixChar)
                    let node = PrefixNode (nextAcc, prefixChar, inner subset nextAcc)
                    prefixChar, node)
            List.concat [ followSet; final ] |> dict
        PrefixNode ("", '\u0000', inner set "")

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
    let internal setIntersection (features: IDictionary<string, Node>) (sets: IDictionary<string, Node>) setIdentifier =
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
    static member fromSetIntersection features sets setIdentifier =
        PrefixTree.setIntersection features sets setIdentifier |> PrefixTree.makeTree
