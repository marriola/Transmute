// Project:     TransmuteLib
// Module:      StateMachine
// Description: Generic state machine. Supports epsilon transitions and backtracking.
// Copyright:   (c) 2023 Matt Arriola
// License:     MIT

namespace TransmuteLib

open TransmuteLib.Utils.Operators

type Origin = From of State

type InputSymbol =
    | OnChar of char
    | OnEpsilon
    | OnAny
with
    member this.Char =
        match this with
        | OnChar c -> c

type Destination = To of State

type TransitionKey = Origin * InputSymbol

type Transition = Origin * InputSymbol * Destination

/// <summary>
/// Represents a transition table.
/// </summary>
/// <typeparam name="TState">The type of states in the state machine.</typeparam>
type TransitionTable = Map<State * InputSymbol, State>

/// <summary>
/// Represents the transitions that can be taken from a state.
/// </summary>
/// <typeparam name="TState">The type of states in the state machine.</typeparam>
/// <typeparam name="TSymbol">The type of symbol in the state machine's language.</typeparam>
type StateTransition =
    { Origin: Origin
      Transitions: (Destination * InputSymbol) list
    }

module internal StateMachine =
    let inline getOrigin (From origin, _, _) = origin
    let inline getInput (_, on, _) = on
    let inline getDest (_, _, To dest) = dest

    /// <summary>
    /// Creates a transition table.
    /// </summary>
    /// <typeparam name="TState">The type of states in the state machine.</typeparam>
    /// <param name="classes">The list of characters and transitions that can be taken from them.</param>
    let createTransitionTableFromClasses<'TState when 'TState : comparison and 'TState : equality>
        (classes: StateTransition list) =
        classes
        |> List.collect (fun { Origin = From origin; Transitions = transitions } ->
            transitions
            |> List.map (fun (To dest, input) -> (origin, input), dest))
        |> Map.ofSeq

    let groupTransitions transitions =
        transitions
        |> List.groupBy getOrigin

    /// <summary>
    /// Creates a group of transitions from a state.
    /// </summary>
    /// <param name="state">The state to transition from.</param>
    /// <param name="transitions">The transitions that can be taken from the state.</param>
    let makeTransitions state transitions =
        { Origin = state
          Transitions = List.ofSeq transitions
        }

    /// <summary>
    /// Creates a list of transitions to a single state on many input symbols.
    /// </summary>
    /// <param name="charsets">A list of character sequences.</param>
    /// <param name="state">The state to transition to.</param>
    let onMany charsets state =
        charsets
        |> Seq.collect (fun cset -> cset |> Seq.map (fun c -> To state, OnChar c))
        |> List.ofSeq

    /// Represents the action to take after transitioning to the error state
    type ErrorAction<'TValue, 'TResult> =
        /// Reprocess the same input from the start state
        | Restart of 'TValue
        /// Stop processing input
        | Stop of 'TResult

    type TransformationTable = Map<State * string * State, string>

    type MachineState<'TValue> =
        { position: int
          currentState: State
          currentValue: 'TValue }

    type OnError<'TValue, 'TResult> = char -> MachineState<'TValue> -> ErrorAction<'TValue, 'TResult>

    type OnTransition<'TValue> = char -> Transition -> MachineState<'TValue> -> 'TValue

    type OnFinish<'TValue, 'TResult> = 'TValue -> 'TResult

    type Config<'TValue, 'TResult> =
        { transitionTable: unit -> TransitionTable
          startState: unit -> State
          errorState: unit -> State
          initialValue: unit -> 'TValue
          fError: unit -> OnError<'TValue, 'TResult>
          fTransition: unit -> OnTransition<'TValue>
          fFinish: unit -> OnFinish<'TValue, 'TResult>
        }

    let private require msg = (fun () -> failwithf "%s required" msg)
    let private provide x = (fun () -> x)

    let stateMachineConfig<'TState, 'TValue, 'TResult when 'TState : equality and 'TState : comparison> () : Config<'TValue, 'TResult> =
        { transitionTable = require "transition table"
          startState = require "start state"
          errorState = require "error state"
          initialValue = require "initial value"
          fError = require "error function"
          fTransition = require "transition function"
          fFinish = require "finish function" }

    let withTransitions table config =
        { config with transitionTable = provide table }

    let withStartState state config =
        { config with startState = provide state }

    let withErrorState state config =
        { config with errorState = provide state }

    let withInitialValue value config =
        { config with initialValue = provide value }

    let onError fError config =
        { config with fError = provide fError }

    let onTransition fTransition config =
        { config with fTransition = provide fTransition }

    let onFinish fFinish config =
        { config with fFinish = provide fFinish }

    let private completeConfig config =
        ( config.transitionTable(),
          config.startState(),
          config.errorState(),
          config.initialValue(),
          config.fError(),
          config.fTransition(),
          config.fFinish() )

    let inline private give x = fun () -> x

    /// <summary>
    /// Runs the specified NFA.
    /// </summary>
    /// <param name="input">The input to iterate over.</param>
    /// <param name="config">The state machine configuration.</param>
    let runNFA<'TValue, 'TResult>
        (input: string)
        (config: Config<'TValue, 'TResult>)
        : 'TResult =
        let ( transitionTable,
              startState,
              errorState,
              initialValue,
              fError,
              fTransition,
              fFinish
            ) = completeConfig config

        // Group the transition table first by state, then by input symbol so we know when an alternate path can be taken
        let transitionTable =
            transitionTable
            |> Map.toList
            |> List.groupBy (fst >> fst)
            |> List.map (fun (key, ts) -> key, List.groupBy (fst >> snd) ts |> Map.ofList)
            |> Map.ofList

        let step currentState inputSymbol =
            let charInput = OnChar inputSymbol
            let transitionsFromCurrent = Map.tryFind currentState transitionTable |> Option.defaultValue Map.empty
            let inputTransitions = Map.tryFind charInput transitionsFromCurrent |> Option.defaultValue []
            let epsilonTransitions = Map.tryFind OnEpsilon transitionsFromCurrent |> Option.defaultValue []
            let anyTransitions = Map.tryFind OnAny transitionsFromCurrent |> Option.defaultValue []

            (inputTransitions @ epsilonTransitions @ anyTransitions)
            |> List.map (fun ((origin, input), dest) -> From origin, input, To dest)

        let rec inner visited stack =
            match stack with
            | (currentValue, _, _, [])::_ ->
                fFinish (currentValue())
            | (currentValue, currentState, position, (nextSymbol::inputRest as input))::stackRest ->
                let currentValue = currentValue()
                let machineState =
                    { position = position
                      currentState = currentState
                      currentValue = currentValue }
                let getNextValue t value = { machineState with currentValue = value } |> fTransition nextSymbol t
                let transitions = step currentState nextSymbol
                let branches =
                    transitions
                    |> List.mapi (fun i ((_, matchSymbol, To nextState) as transition) ->
                        let isEpsilonTransition = matchSymbol = OnEpsilon
                        let nextPosition, nextInput = if isEpsilonTransition then position, input else position + 1, inputRest
                        //if i > 0 then printf " "
                        (getNextValue transition, nextState, nextPosition, nextInput))

                if List.isEmpty transitions then
                    let nextPosition = position + 1
                    let nextInput = input[1..]
                    let matchSymbol = OnChar nextSymbol

                    match fError nextSymbol machineState with
                    | Stop result ->
                        result
                    | Restart value ->
                        if stackRest.Length > 0 && not (State.isFinal currentState) then
                            match stackRest with
                            | (_, s, _, _)::_ ->
                                // Backtrack to the next branch if there is one and we haven't already been there
                                //printfn "*** BACKTRACK to %O,%O - %d left ***" matchSymbol s (List.length stackRest - 1)
                                if Set.contains (matchSymbol, s) visited then
                                    inner visited stackRest[1..]
                                else
                                    inner (Set.add (matchSymbol, s) visited) stackRest
                            | _ ->
                                fFinish value
                        elif currentState <> startState then
                            // Reprocess the same input unless we're on the start state
                            inner Set.empty ((give value, startState, position, input) :: stackRest)
                        elif not (List.isEmpty inputRest) then
                            // Process the next input if there is one
                            inner Set.empty ((give value, startState, nextPosition, nextInput) :: stackRest)
                        else
                            // Nothing left, just finish
                            fFinish value
                else
                    let (getNextValue, nextState, nextPosition, nextInput) :: alternateBranches = branches
                    let alternateBranches =
                        alternateBranches
                        |> List.map (fun (getNextValue, nextState, nextPosition, nextInput) ->
                            (fun () -> getNextValue currentValue), nextState, nextPosition, nextInput)
                    let nextValue = fun () -> getNextValue currentValue
                    let stackRest = alternateBranches @ stackRest
                    let stackTop = nextValue, nextState, nextPosition, nextInput
                    inner visited (stackTop :: stackRest)

        inner Set.empty [ (give initialValue, startState, 0, List.ofSeq input) ]

    /// <summary>
    /// Runs the specified DFA.
    /// </summary>
    /// <param name="input">The input to iterate over.</param>
    /// <param name="config">The state machine configuration.</param>
    let runDFA<'TValue, 'TResult>
        input
        (config: Config<'TValue, 'TResult>)
        : 'TResult =
        let ( transitionTable,
              startState,
              errorState,
              initialValue,
              fError,
              fTransition,
              fFinish
            ) = completeConfig config

        let step currentState inputSymbol =
            let transition = Map.tryFind (currentState, OnChar inputSymbol) transitionTable
            let transitionOnEpsilon = Map.tryFind (currentState, OnEpsilon) transitionTable
            let transitionOnAny = Map.tryFind (currentState, OnAny) transitionTable
            let next =
                match transition, transitionOnEpsilon, transitionOnAny with
                | Some t, _, _ -> Some (OnChar inputSymbol, t)
                | _, Some t, _ -> Some (OnEpsilon, t)
                | _, _, Some t -> Some (OnAny, t)
                | _ -> None
            next
            |> Option.map (fun (inputSymbol, dest) -> From currentState, inputSymbol, To dest)
            

        let rec inner currentValue currentState position input =
            match input with
            | [] ->
                fFinish currentValue
            | nextSymbol::rest ->
                let machineState =
                    { position = position
                      currentState = currentState
                      currentValue = currentValue }
                let transition = step currentState nextSymbol
                let nextState = transition |> Option.map (fun (_, _, To dest) -> dest) |> Option.defaultValue errorState
                let isEpsilonTransition = transition |> Option.map (fun (_, matchSymbol, _) -> matchSymbol = OnEpsilon) |> Option.defaultValue false
                let nextInput = if isEpsilonTransition then input else rest

                if transition = None then
                    match fError nextSymbol machineState with
                    | Restart value when currentState <> startState ->
                        // Reprocess the same input unless we're on the start state
                        inner value startState position input
                    | Restart value when rest <> [] ->
                        // Process the next input if there is one
                        inner value startState (position + 1) nextInput
                    | Restart value ->
                        // Nothing left, just finish
                        fFinish value
                    | Stop result ->
                        result
                else
                    let transition = Option.get transition
                    inner (fTransition nextSymbol transition machineState) nextState (position + 1) nextInput

        input
        |> List.ofSeq
        |> inner initialValue startState 0
