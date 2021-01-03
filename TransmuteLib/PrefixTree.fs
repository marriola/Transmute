namespace TransmuteLib

open System.Collections.Generic
open TransmuteLib

type private PrefixTree =
    | Root of children:PrefixTree list
    | Node of prefix:string * value:char * children:PrefixTree list
    | Leaf of utterance:string

module private PrefixTree =
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

    let private isEmpty = System.String.IsNullOrEmpty

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

type private PrefixTree with
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
    static member fromSetIntersection (features: Map<string, Node>) (sets: Map<string, Node>) setDescriptor =
        let getVal (kvp: KeyValuePair<'a, 'b>) = kvp.Value
        let alphabet =
            Node.getAlphabet
                (Seq.map getVal features |> List.ofSeq)
                (Seq.map getVal sets |> List.ofSeq)
        Node.setIntersection alphabet features sets setDescriptor |> PrefixTree.makeTree
