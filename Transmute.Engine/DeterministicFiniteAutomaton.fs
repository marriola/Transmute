// Project:     Transmute.Engine
// Module:      DeterministicFiniteAutomaton
// Description: Converts a finite state transducer that is nondeterministic to an equivalent one that is deterministic.
// Copyright:   (c) 2026 Matt Arriola
// License:     MIT

namespace Transmute.Engine

open System.Collections.Generic

type Transformation = Transition * TransitionResult

module private DeterministicFiniteAutomaton =
    /// Returns true if the states X and Y are equivalent, or if state Y is a merged state containing state X
    let inline private ( <% ) x y =
        match x, y with
        | _ when x = y ->
            true
        | (State _ as s), MergedState children
        | MergedState children, (State _ as s) when Array.contains s children ->
            true
        | _ ->
            false

    /// Transition augmented with an optional transformation
    type private Transition' = Transition * TransitionResult

    // Combines a transition list and a transformation list into a transition + optional transformation list
    let private augment (transitions: Transition list) (transformations: Transformation list) =
        let transformationsByTransition =
            transformations
            |> List.groupBy (fun (transition, _) -> transition)
            |> List.map (fun (key, tfs) -> (key, List.map snd tfs))
            |> Map.ofList
        transitions
        |> List.map (fun t ->
            let transformation =
                match Map.tryFind t transformationsByTransition with
                | None -> OutputDefault
                | Some [tf] -> tf
#if DEBUG
                | Some tfs -> failwithf "Transition %O has %d transformations; it should have 0 or 1" t tfs.Length
#endif
            t, transformation)

    let private transitionsFrom state transitions =
        transitions
        |> List.choose (fun (((From origin, _, _), _) as t) ->
            if state <% origin then Some t else None)

    let inline private getOrigin (transition, _) = StateMachine.getOrigin transition
    let inline private getDest (transition, _) = StateMachine.getDest transition
    let inline private getInput (transition, _) = StateMachine.getInput transition

    let printNfa table =
        printf "NFA:\n\n"
        table
        |> List.sortBy (fun ((From origin, input, dest), result) -> (State.ord origin, input, dest), result)
        |> List.indexed
        |> List.map (fun (i, ((From origin, input, To dest), result)) ->
            let t = sprintf "(%O, %O)" origin input
            sprintf "%d.\t%-25s-> %O, %O" i t dest result)
        |> String.concat "\n"
        |> System.Console.WriteLine

    /// Converts an NFA into an equivalent DFA.
    let fromNfa startState errorState (table: Transition list) (transformations: Transformation list) showNfa =
        let table = augment table transformations

        /// <summary>
        /// Computes the list of transitions that can be taken from a state, skipping over epsilon transitions.
        /// </summary>
        /// <returns>A set of input symbol and state tuples.</returns>
        let computePowerSet (originalResult: TransitionResult) state =
            let rec inner states result =
                match states with
                | [] ->
                    List.rev result
                | x::xs ->
                    // States to which we can ε transition from x
                    let followStates =
                        table
                        |> List.filter (function
                            | (From origin, OnEpsilon, _), _ when origin = x -> true
                            | _ -> false)
                        |> List.map getDest
                        |> set
                    // Non-ε transitions we can take from those states
                    let followTransitions =
                        table
                        |> List.filter
                            (fun ((From origin, input, _), _) ->
                                input <> OnEpsilon
                                && (origin <% x
                                    || Set.contains origin followStates))
                        |> List.map (fun ((_, input, To dest), result) ->
                            input, dest, TransitionResult.coalesce originalResult result)
                    let nextStates =
                        followStates
                        |> Seq.filter (fun s ->
                            table
                            |> List.exists (function
                                | (From origin, OnEpsilon, _), _ when origin = s -> true
                                | _ -> false))
                        |> List.ofSeq
                    inner (nextStates @ xs) (followTransitions @ result)
            inner [state] []

        let initialInsertion =
            table
            |> List.tryFind (fun ((From origin, inputSymbol, _), _) -> origin = startState && inputSymbol = OnEpsilon)
            |> Option.map (fun (_, result) -> result)
            |> Option.defaultValue OutputDefault

        let followDestinationAcc = new HashSet<Transition * TransitionResult>()
        let mutable search = new HashSet<Transition * TransitionResult>()
        let mutable nextSearch = new HashSet<Transition * TransitionResult>()

        let followDestination current transitions =
            followDestinationAcc.Clear()
            nextSearch.Clear()
            search.Clear()
            search.UnionWith transitions
            
            while search.Count > 0 do
                for (_, input, To d), tResult in search do
                    for (From successor, OnEpsilon, To d2), uResult in table do
                        if successor <% d then
                            let transitionToFollow = (From current, input, To d2), TransitionResult.coalesce uResult tResult

                            if not (search.Contains transitionToFollow) then
                                nextSearch.Add transitionToFollow |> ignore

                    let mutable partiallyDeterministic = false
                    let mutable completelyDeterministic = true

                    for entry in table do
                        if (partiallyDeterministic = false || completelyDeterministic = true) then
                            let (From origin, input, _), _ = entry

                            if d = origin then
                                if input = OnEpsilon then
                                    completelyDeterministic <- false
                                else
                                    partiallyDeterministic <- true

                    if completelyDeterministic || partiallyDeterministic then
                        followDestinationAcc.Add ((From current, input, To d), tResult) |> ignore

                let temp = search
                search <- nextSearch
                nextSearch <- temp
                nextSearch.Clear()

            List.ofSeq followDestinationAcc

        /// <summary>
        /// For each transition, get the states that can be reached from its destination by
        /// non-epsilon transitions, and create transitions to them from the given state.
        /// </summary>
        /// <returns>A list of deterministric transitions.</returns>
        let followEpsilonTransitions origin originalResult transitions =
            let originIsStartState = State.name origin = "S"
            transitions
            |> List.collect (getDest >> computePowerSet originalResult)
            |> List.distinct
            |> List.map (fun (input, dest, result) ->
                let dest =
                    if input = OnAny && originIsStartState then
                        origin
                    else
                        dest
                (From origin, input, To dest), result)

        /// Replaces epsilon transitions originating from current with all possible deterministic transitions.
        ///
        /// Also replaces destination states that have epsilon transition with states that can be reached deterministically.
        let removeNondeterminism current initialInsertion transitions = 
            // Separate epsilon and non-epsilon transitions
            let epsilonTransitions, nonEpsilonTransitions =
                transitions
                |> List.partition (getInput >> (=) OnEpsilon)
            // Follow epsilon transitions to the next state with non-epsilon transitions
            let followedEpsilonTransitions = followEpsilonTransitions current initialInsertion epsilonTransitions
            // Combine with followed epsilon transitions, and follow the destination state if it has epsilon transitions.
            let transitions =
                nonEpsilonTransitions @ followedEpsilonTransitions
                |> followDestination current
            transitions

        /// Groups all transitions by input symbol, and merges states that can be reached
        /// by the same input symbol.
        let groupTransitions current (transitions: (Transition * TransitionResult) list) =
            let single, multiple =
                transitions
                |> List.distinct
                |> List.groupBy getInput
                |> List.partition (snd >> List.length >> (=) 1)
            let single =
                single
                |> List.collect snd
                |> List.map (function
                    // Take any merged states looping back to themselves on a catch-all transition that break out of the merged
                    // state, and redirect them back to the merged state.
                    | (From (MergedState _ as origin), OnAny, To dest), result when dest <% origin ->
                        (From origin, OnAny, To origin), result
                    | t ->
                        t)
            let merged =
                multiple
                |> List.map (fun (on, dests) ->
                    let mergedDest =
                        dests
                        |> List.map getDest
                        |> State.merge
                    let production =
                        (OutputDefault, dests)
                        ||> List.fold (fun out (_, result) ->
                            match out, result with
                            | OutputDefault, (_ as result) ->
                                result
#if DEBUG
                            | ReplacesWith (_, a), ReplacesWith (_, b) when a <> b ->
                                failwithf "Merged state %O has multiple productions! (%O, %O)" mergedDest out result
#endif
                            | _ ->
                                out)
                    (From current, on, To mergedDest), production)
            single @ merged

        let rec fromNfa' initialInsertion searchStack dfaTransitions =
            match searchStack with
            | [] -> 
                dfaTransitions
                |> List.ofSeq
            | x::rest when x = errorState ->
                fromNfa' initialInsertion rest dfaTransitions
            | current::rest ->
                // transitions from current state -> skip lambdas -> group by symbol
                let transitionsFromCurrent =
                    table
                    |> transitionsFrom current
                    |> removeNondeterminism current initialInsertion
                    |> groupTransitions current
                // Follow transitions that don't go to the current state or a state already in the stack
                let nextStack =
                    transitionsFromCurrent
                    |> List.map getDest
                    |> List.where ((<>) current)
                    |> List.append rest
                    |> List.distinct
                let nextTransitions =
                    transitionsFromCurrent
                    |> Set.ofList
                    |> Set.union dfaTransitions
                fromNfa' OutputDefault nextStack nextTransitions

        if showNfa then printNfa table

        fromNfa' initialInsertion [startState] Set.empty
