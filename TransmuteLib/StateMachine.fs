// Project:     TransmuteLib
// Module:      StateMachine
// Description: Generic state machine. Supports epsilon transitions and backtracking.
// Copyright:   (c) 2023 Matt Arriola
// License:     MIT

namespace TransmuteLib

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

module private StateMachine =
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
        /// Jump to the start state and reprocess the same input symbol
        | Restart of 'TValue
        /// Continue processing from the next input symbol.
        | Continue of 'TValue
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

        let inline step currentState inputSymbol =
            match Map.tryFind (currentState, OnChar inputSymbol) transitionTable with
            | Some dest -> Some (From currentState, OnChar inputSymbol, To dest)
            | _ ->
            match Map.tryFind (currentState, OnEpsilon) transitionTable with
            | Some dest -> Some (From currentState, OnEpsilon, To dest)
            | _ ->
            match Map.tryFind (currentState, OnAny) transitionTable with
            | Some dest (* when not (Special.SyllableBoundarySymbols.Contains inputSymbol) *) -> Some (From currentState, OnAny, To dest)
            | _ -> None

        let rec inner currentValue currentState position input =
            match input with
            | [] ->
                fFinish currentValue
            | nextSymbol::rest ->
                let transition = step currentState nextSymbol

                let nextState, nextInput =
                    match transition with
                    | Some (_, matchSymbol, To dest) ->
                        dest, if matchSymbol = OnEpsilon then input else rest
                    | _ ->
                        errorState, rest

                let nextPosition =
                    if Special.Symbols.Contains nextSymbol then
                        position
                    else
                        position + 1

                let machineState =
                    { position = position
                      currentState = currentState
                      currentValue = currentValue }

                if transition = None then
                    match fError nextSymbol machineState with
                    | Restart value when currentState <> startState ->
                        // Reprocess the same input unless we're on the start state
                        inner value startState position input
                    | Restart value when rest <> [] ->
                        // Process the next input if there is one
                        inner value startState nextPosition nextInput
                    | Restart value ->
                        // Nothing left, just finish
                        fFinish value
                    | Continue value ->
                        inner value currentState nextPosition nextInput
                    | Stop result ->
                        result
                else
                    let transition = Option.get transition
                    inner (fTransition nextSymbol transition machineState) nextState nextPosition nextInput

        input
        |> List.ofSeq
        |> inner initialValue startState 0
