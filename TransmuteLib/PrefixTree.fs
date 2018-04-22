module PrefixTree

open System.Collections.Generic
open System
open TransmuteLib
open TransmuteLib.RuleParser

type PrefixTree =
    | Node of prefix:string * value:char * children:Dictionary<char, PrefixTree>

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
    let rec inner (root: PrefixTree) (subtree: PrefixTree) (accumulator: string) (utterance: string) =
        if utterance.Length = 0 then
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
            | Node (_, _, children) ->
                if children.ContainsKey headSymbol then
                    inner root children.[headSymbol] next rest
                else
                    let nextSubtree = Node(next, headSymbol, new Dictionary<char, PrefixTree>())
                    children.[headSymbol] <- inner nextSubtree nextSubtree next rest
                    root
    let root = Node ("", NUL, new Dictionary<char, PrefixTree>())
    let rec loopOverSet (set: string list) =
        if set.IsEmpty then
            root
        else
            let suffixedUtterance = set.Head + (string NUL)
            ignore (inner root root "" suffixedUtterance)
            loopOverSet set.Tail
    loopOverSet set

type PrefixTree with
    /// <summary>
    /// Creates a prefix tree from the members of a feature
    /// </summary>
    /// <param name="feature"></param>
    /// <param name="isPresent"></param>
    static member fromFeature feature isPresent =
        makeTree (getFeatureMembers feature isPresent)

    /// <summary>
    /// Creates a prefix tree from the members of a set.
    /// </summary>
    /// <param name="set"></param>
    static member fromSet set =
        makeTree (getSetMembers set)

    /// <summary>
    /// Creates a prefix tree from the intersection of a list of sets and features.
    /// </summary>
    /// <param name="sets">The available sets.</param>
    /// <param name="features">The available features.</param>
    /// <param name="setIdentifier">The SetIdentifierNode listing the sets and features to intersect.</param>
    static member fromSetIntersection sets features setIdentifier =
        setIntersection sets features setIdentifier |> makeTree
