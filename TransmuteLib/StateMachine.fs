namespace TransmuteLib

open System.Collections.Generic

/// Represents how the the input symbol is matched to a transition. 
type Match =
    /// The input symbol must match a character
    | Char of char

    /// If no other transition applies, take the transition without consuming an input symbol.
    | Epsilon

    /// If no other transition applies, accept any input symbol.
    | Any

type Transition<'TState> = ('TState * Match) * 'TState

/// <summary>
/// Represents a transition table.
/// </summary>
/// <typeparam name="TState">The type of states in the state machine.</typeparam>
type TransitionTable<'TState> = IDictionary<'TState * Match, 'TState>

/// <summary>
/// Represents the transitions that can be taken from a state.
/// </summary>
/// <typeparam name="TState">The type of states in the state machine.</typeparam>
/// <typeparam name="TSymbol">The type of symbol in the state machine's language.</typeparam>
type StateTransition<'TState, 'TSymbol> =
    { state: 'TState;
      transitions: ('TState * 'TSymbol) list;
    }

/// <summary>
/// Represents the transitions that can be taken from a state in a state machine that
/// iterates over characters.
/// </summary>
/// <typeparam name="TState">The type of states in the state machine.</typeparam>
type CharStateTransition<'TState> = StateTransition<'TState, Match>

module StateMachine =
    /// <summary>
    /// Creates a transition table.
    /// </summary>
    /// <typeparam name="TState">The type of states in the state machine.</typeparam>
    /// <param name="classes">The list of characters and transitions that can be taken from them.</param>
    let createTransitionTableFromClasses<'TState, 'TSymbol when 'TState : equality and 'TSymbol : equality> (classes: StateTransition<'TState, 'TSymbol> list) =
        classes
        |> List.collect (fun c ->
            c.transitions
            |> List.map (fun (dest, on) -> (c.state, on), dest))
        |> dict

    let groupTransitions transitions =
        transitions
        |> List.groupBy (fun t -> fst t)
        //|> List.map (fun (key, lst) -> key, lst |> List.map snd)

    let createTransitionTable<'TState, 'TSymbol when 'TState : equality and 'TSymbol : equality> (transitions: Transition<'TState> list) =
        groupTransitions transitions |> dict

    /// <summary>
    /// Creates a group of transitions from a state.
    /// </summary>
    /// <param name="state">The state to transition from.</param>
    /// <param name="transitions">The transitions that can be taken from the state.</param>
    let transitionFrom state transitions =
        { state = state;
          transitions = List.ofSeq transitions
        }

    /// <summary>
    /// Creates a transition to a state on an input symbol.
    /// </summary>
    /// <param name="c">The input symbol.</param>
    /// <param name="state">The state to transition to.</param>
    let on c state = (state, Char c)

    /// <summary>
    /// Creates a list of transitions to a state from many input symbols.
    /// </summary>
    /// <param name="charsets">A list of character sequences.</param>
    /// <param name="state">The state to transition to.</param>
    let onMany charsets state =
        charsets
        |> Seq.collect (fun cset -> cset |> Seq.map (fun c -> (state, Char c)))
        |> List.ofSeq

    /// <summary>
    /// Creates a transition to a state on an input symbol.
    /// </summary>
    /// <param name="state">The state to transition to.</param>
    /// <param name="c">The input symbol.</param>
    let transitionTo state c = state, Char c

    /// <summary>
    /// Creates a transition to a state on any symbol not already matched.
    /// </summary>
    /// <param name="state">The state to transition to.</param>
    let anyTo state = state, Any

    /// <summary>
    /// Creates a transition to a state that does not consume an input symbol.
    /// </summary>
    /// <param name="state">The state to transition to.</param>
    let epsilonTo state = state, Epsilon

    /// <summary>
    /// Compute the result of transitioning from the current state on an input symbol.
    /// </summary>
    /// <param name="transitionTable">The transition table.</param>
    /// <param name="currentState">The current state to transition from.</param>
    /// <param name="inputSymbol">The input symbol to transition on.</param>
    /// <param name="errorState">The error state to transition to if no other transition can be taken.</param>
    let step errorState (transitionTable: TransitionTable<'a>) currentState inputSymbol =
        let transition = currentState, Char inputSymbol
        let transitionOnEpsilon = currentState, Epsilon
        let transitionOnAny = currentState, Any

        if transitionTable.ContainsKey(transition) then
            Char inputSymbol, transitionTable.[transition]

        else if transitionTable.ContainsKey(transitionOnEpsilon) then
            Epsilon, transitionTable.[transitionOnEpsilon]

        else if transitionTable.ContainsKey(transitionOnAny) then
            Any, transitionTable.[transitionOnAny]

        else
            Char inputSymbol, errorState

    type ErrorAction<'TValue, 'TResult> =
        | Restart of 'TValue
        | Stop of 'TResult

    type Config<'TState, 'TValue, 'TResult when 'TState : equality> =
        { transitionTable: unit -> TransitionTable<'TState>
          startState: unit -> 'TState
          errorState: unit -> 'TState
          initialValue: unit -> 'TValue
          fError: unit -> (char -> 'TState -> 'TValue -> ('TValue -> 'TValue ) -> ErrorAction<'TValue, 'TResult>)
          fTransition: unit -> (bool -> char -> 'TState -> 'TState -> 'TValue -> 'TValue)
          fFinish: unit -> ('TValue -> 'TResult)
        }

    let private require msg = (fun () -> failwithf "%s required" msg)
    let private provide x = (fun () -> x)

    let stateMachineConfig<'TState, 'TValue, 'TResult when 'TState : equality> () : Config<'TState, 'TValue, 'TResult> =
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
    /// Runs the specified state machine. 
    /// </summary>
    /// <param name="input">The input to iterate over.</param>
    /// <param name="config">The state machine configuration.</param>
    let runStateMachine<'TState, 'TValue, 'TResult when 'TState : equality>
        input
        (config: Config<'TState, 'TValue, 'TResult>)
        : 'TResult =
        let ( transitionTable,
              startState,
              errorState,
              initialValue,
              fError,
              fTransition,
              fFinish
            ) = completeConfig config

        let rec inner currentValue currentState input =
            match input with
            | [] ->
                fFinish currentValue
            | nextSymbol::rest ->
                let matchSymbol, nextState = step errorState transitionTable currentState nextSymbol
                let isEpsilonTransition = matchSymbol = Epsilon
                let nextInput = if isEpsilonTransition then input else rest
                let getNextValue = fTransition isEpsilonTransition nextSymbol currentState nextState

                if nextState = errorState then
                    match fError nextSymbol currentState currentValue getNextValue with
                    | Restart value when currentState <> startState ->
                        inner value startState input
                    | Restart value when rest <> [] ->
                        inner value startState nextInput
                    | Restart value ->
                        fFinish value
                    | Stop result ->
                        result
                else
                    inner (getNextValue currentValue) nextState nextInput

        input
        |> List.ofSeq
        |> inner initialValue startState
