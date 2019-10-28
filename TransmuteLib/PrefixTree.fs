namespace TransmuteLib

open System.Collections.Generic
open TransmuteLib
open TransmuteLib.Node
open TransmuteLib.Exceptions

type PrefixTree =
    | Root of children:PrefixTree list
    | Node of prefix:string * value:char * children:PrefixTree list
    | Leaf of utterance:string

module PrefixTree =
    // maketree [ "k"; "kw"; "g"; "gw"; "p"; "b"; "t"; "d" ]

    //                Ø
    //                |
    //    +--------+----+--+--+--+
    //    |        |    |  |  |  |
    //    k        g    p  b  t  d
    // w / \ λ  w / \ λ
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
                    then [ Leaf acc ]
                    else List.empty
            let followSet =
                set
                |> List.where (not << isEmpty)
                |> List.groupBy Seq.head
                |> List.map (fun (prefixChar, subset) ->
                    let subset = subset |> List.map (fun s -> s.[1..])
                    let nextAcc = acc + (string prefixChar)
                    Node (nextAcc, prefixChar, inner subset nextAcc))
            followSet @ final
        Root (inner set "")

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
    let internal setIntersection (alphabet: Set<string>) (features: Map<string, Node>) (sets: Map<string, Node>) setIdentifier =
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
                    match Node.untag x with
                    | TermIdentifierNode name
                    | SetIdentifierNode name ->
                        tryFindSetOrFeature (fun _ -> getSetMembers sets.[name]) "Set" x name
                        |> set
                        |> addToSet true
                    | FeatureIdentifierNode (isPresent, name) ->
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
                    | _ ->
                        let position, node = Node.untagWithMetadata x
                        invalidSyntax (sprintf "Unexpected token '%O'" node) position
                inner xs nextSet
        inner setIdentifier alphabet |> List.ofSeq

type PrefixTree with
    /// <summary>
    /// Creates a prefix tree from the members of a feature
    /// </summary>
    /// <param name="feature"></param>
    /// <param name="isPresent"></param>
    static member fromFeature feature isPresent =
        PrefixTree.makeTree (Node.getFeatureMembers isPresent feature)

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
    /// <param name="setIdentifier">The CompoundSetIdentifierNode listing the sets and features to intersect.</param>
    static member fromSetIntersection (features: Map<string, Node>) (sets: Map<string, Node>) setIdentifier =
        let getVal (kvp: KeyValuePair<'a, 'b>) = kvp.Value
        let alphabet =
            Node.getAlphabet
                (Seq.map getVal features |> List.ofSeq)
                (Seq.map getVal sets |> List.ofSeq)
        PrefixTree.setIntersection alphabet features sets setIdentifier |> PrefixTree.makeTree
