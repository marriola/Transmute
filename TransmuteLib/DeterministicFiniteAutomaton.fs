// Project:     TransmuteLib
// Module:      DeterministicFiniteAutomaton
// Description: Converts a finite state transducer that is nondeterministic to an equivalent one that is deterministic.
// Copyright:   (c) 2023 Matt Arriola
// License:     MIT

namespace TransmuteLib

type Transformation = Transition * TransitionResult

type GenericList<'T> = System.Collections.Generic.List<'T>
type GenericSet<'T> = System.Collections.Generic.HashSet<'T>
type GenericStack<'T> = System.Collections.Generic.Stack<'T>

module internal DeterministicFiniteAutomaton =
    type private TransitionType =
        /// A transition with a destination state that needs to be checked for deterministic transitions.
        | MaybeDeterministic of (Transition * TransitionResult)
        /// A transition with a destination state that either has deterministic transitions or just doesn't have any nondeterministic ones.
        | Deterministic of (Transition * TransitionResult)

    /// Returns true if the states X and Y are equivalent, or if state Y is a merged state containing state X
    let inline private ( <% ) x y =
        match x, y with
        | _ when x = y ->
            true
        | (State _ as s), MergedState children
        | MergedState children, (State _ as s) when List.contains s children ->
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
                | Some tfs -> failwithf "Transition %O has %d transformations; it should have 0 or 1" t tfs.Length
            t, transformation)

    let private transitionsFrom state transitions =
        transitions
        |> List.choose (fun (((From origin, _, _), _) as t) ->
            if state <% origin then Some t else None)

    let inline private getOrigin (transition, _) = StateMachine.getOrigin transition
    let inline private getDest (transition, _) = StateMachine.getDest transition
    let inline private getInput (transition, _) = StateMachine.getInput transition

    /// <summary>
    /// Computes the list of transitions that can be taken from a state, skipping over epsilon transitions.
    /// </summary>
    /// <returns>A set of input symbol and state tuples.</returns>
    let private computePowerSet transitions (originalResult: TransitionResult) state =
        let rec inner transitions states result =
            match states with
            | [] ->
                List.rev result
            | x::xs ->
                // States to which we can ε transition from x
                let followStates =
                    transitions
                    |> List.filter (function
                        | (From origin, OnEpsilon, _), _ when origin = x -> true
                        | _ -> false)
                    |> List.map getDest
                    |> set
                // Non-ε transitions we can take from those states
                let followTransitions =
                    transitions
                    |> List.filter
                        (fun ((From origin, input, _), _) ->
                            input <> OnEpsilon
                            && (origin <% x
                                || Set.contains origin followStates))
                    |> List.map (fun ((_, input, To dest), result) -> input, dest, originalResult.Or result)
                let nextStates =
                    followStates
                    |> Seq.filter (fun s ->
                        transitions
                        |> List.exists (function
                            | (From origin, OnEpsilon, _), _ when origin = s -> true
                            | _ -> false))
                    |> List.ofSeq
                inner transitions (nextStates @ xs) (followTransitions @ result)
        inner transitions [state] []

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

        let initialInsertion =
            table
            |> List.tryFind (fun ((From origin, inputSymbol, _), _) -> origin = startState && inputSymbol = OnEpsilon)
            |> Option.map (fun (_, result) -> result)
            |> Option.defaultValue OutputDefault

        let inline hasNonEpsilonTransition state =
            table
            |> List.exists (fun ((From origin, input, _), _) -> state = origin && input <> OnEpsilon)

        let inline allTransitionsDeterministic origin =
            table
            |> List.exists (function
                | (From o, OnEpsilon, _), _ when o = origin -> true
                | _ -> false)
            |> not

        // TODO: refactor this 8 level indented beast
        /// <summary>
        /// Recursively follows the destination of each transition to a state that has non-epsilon
        /// transitions, eliminating any that have only epsilon transitions.
        /// </summary>
        /// <returns>A list of deterministic transitions.</returns>
        let followDestination current transitions =
            let rec inner acc search =
                match search with
                | [] -> List.ofSeq acc
                | _ ->
                    // Search for states that might have deterministic transitions from their destinations.
                    let successors =
                        search
                        |> List.collect (fun ((From o, input, To d), tResult as t) ->
                            // For each transition T from O to D that is succeeded by a nondeterministic transition U,
                            // move the destination of T forwards to skip it. If any of these lead to deterministic transitions,
                            // they will be accumulated in the next iteration, along with final states.
                            let followedTransitions =
                                table
                                |> List.choose (function
                                    | (From successor, OnEpsilon, To d2), uResult as u
                                        when successor <% d ->
                                        let result =
                                            match tResult, uResult with
                                            | OutputDefault, x
                                            | x, OutputDefault ->
                                                x
                                            | x, y when x <> y ->
                                                failwith "Both transitions have transformation!"
                                            | _ ->
                                                    tResult
                                        Some (MaybeDeterministic ((From current, input, To d2), result))
                                    | _ ->
                                        None)
                            // Keep the original transition T if D is final or has deterministic transitions
                            let originalTransition =
                                if (allTransitionsDeterministic d || hasNonEpsilonTransition d) then // && (State.isFinal d || input <> OnEpsilon) then
                                    [ Deterministic ((From current, input, To d), tResult) ]
                                else
                                    []
                            originalTransition @ followedTransitions)
                    // Follow transitions to states we haven't already been to that have non-deterministic transitions
                    let nextTransitions =
                        successors
                        |> Seq.choose (function
                            | MaybeDeterministic u when not (List.contains u search) -> Some u
                            | _ -> None)
                        |> List.ofSeq
                    // Accumulate transitions to states that are final or have deterministic transitions
                    let nextAcc =
                        successors
                        |> Seq.choose (function
                            | Deterministic t -> Some t
                            | _ -> None)
                        |> Set.ofSeq
                        |> Set.union acc
                    inner nextAcc nextTransitions

            inner (Set.empty) transitions

        /// <summary>
        /// For each transition, get the states that can be reached from its destination by
        /// non-epsilon transitions, and create transitions to them from the given state.
        /// </summary>
        /// <returns>A list of deterministric transitions.</returns>
        let followEpsilonTransitions origin originalResult transitions =
            transitions
            |> List.collect (getDest >> computePowerSet table originalResult)
            |> List.distinct
            |> List.map (fun (input, dest, result) -> (From origin, input, To dest), result)

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
            let single = List.collect snd single
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
                            | ReplacesWith (_, a), ReplacesWith (_, b) when a <> b ->
                                failwithf "Merged state %O has multiple productions! (%O, %O)" mergedDest out result
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
