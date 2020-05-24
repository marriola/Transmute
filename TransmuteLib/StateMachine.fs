namespace TransmuteLib

type Origin<'TState> = From of 'TState

type InputSymbol =
    | OnChar of char
    | OnEpsilon
    | OnAny

type Destination<'TState> = To of 'TState

type TransitionKey<'TState> = Origin<'TState> * InputSymbol

type Transition<'TState> = Origin<'TState> * InputSymbol * Destination<'TState>

/// <summary>
/// Represents a transition table.
/// </summary>
/// <typeparam name="TState">The type of states in the state machine.</typeparam>
type TransitionTable<'TState when 'TState : comparison> = Map<'TState * InputSymbol, 'TState>

/// <summary>
/// Represents the transitions that can be taken from a state.
/// </summary>
/// <typeparam name="TState">The type of states in the state machine.</typeparam>
/// <typeparam name="TSymbol">The type of symbol in the state machine's language.</typeparam>
type StateTransition<'TState> =
    { Origin: Origin<'TState>
      Transitions: (Destination<'TState> * InputSymbol) list
    }

module StateMachine =
    let getOrigin (From origin, _, _) = origin
    let getInput (_, on, _) = on
    let getDest (_, _, To dest) = dest

    /// <summary>
    /// Creates a transition table.
    /// </summary>
    /// <typeparam name="TState">The type of states in the state machine.</typeparam>
    /// <param name="classes">The list of characters and transitions that can be taken from them.</param>
    let createTransitionTableFromClasses<'TState when 'TState : comparison and 'TState : equality>
        (classes: StateTransition<'TState> list) =
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

    /// <summary>
    /// Compute the result of transitioning from the current state on an input symbol.
    /// </summary>
    /// <param name="transitionTable">The transition table.</param>
    /// <param name="currentState">The current state to transition from.</param>
    /// <param name="inputSymbol">The input symbol to transition on.</param>
    /// <param name="errorState">The error state to transition to if no other transition can be taken.</param>
    let step errorState (transitionTable: TransitionTable<'a>) currentState inputSymbol =
        let transition = Map.tryFind (currentState, OnChar inputSymbol) transitionTable
        let transitionOnEpsilon = Map.tryFind (currentState, OnEpsilon) transitionTable
        let transitionOnAny = Map.tryFind (currentState, OnAny) transitionTable
        let inputSymbol, dest =
            match transition, transitionOnEpsilon, transitionOnAny with
            | Some t, _, _ -> OnChar inputSymbol, t
            | _, Some t, _ -> OnEpsilon, t
            | _, _, Some t -> OnAny, t
            | _ -> OnChar inputSymbol, errorState
        From currentState, inputSymbol, To dest

    /// Represents the action to take after transitioning to the error state
    type ErrorAction<'TValue, 'TResult> =
        /// Reprocess the same input from the start state
        | Restart of 'TValue
        /// Stop processing input
        | Stop of 'TResult

    type TransformationTable<'TState when 'TState : comparison> = Map<'TState * string * 'TState, string>

    type OnError<'TState, 'TValue, 'TResult> = int -> char -> 'TState -> 'TValue -> ('TValue -> 'TValue ) -> ErrorAction<'TValue, 'TResult>

    type OnTransition<'TState, 'TValue> = int -> Transition<'TState> -> bool -> char -> 'TState -> 'TState -> 'TValue -> 'TValue

    type OnFinish<'TValue, 'TResult> = 'TValue -> 'TResult

    type Config<'TState, 'TValue, 'TResult when 'TState : equality and 'TState : comparison> =
        { transitionTable: unit -> TransitionTable<'TState>
          startState: unit -> 'TState
          errorState: unit -> 'TState
          initialValue: unit -> 'TValue
          fError: unit -> OnError<'TState, 'TValue, 'TResult>
          fTransition: unit -> OnTransition<'TState, 'TValue>
          fFinish: unit -> OnFinish<'TValue, 'TResult>
        }

    let private require msg = (fun () -> failwithf "%s required" msg)
    let private provide x = (fun () -> x)

    let stateMachineConfig<'TState, 'TValue, 'TResult when 'TState : equality and 'TState : comparison> () : Config<'TState, 'TValue, 'TResult> =
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
    let runStateMachine<'TState, 'TValue, 'TResult when 'TState : equality and 'TState : comparison>
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

        let rec inner currentValue currentState position input =
            match input with
            | [] ->
                fFinish currentValue
            | nextSymbol::rest ->
                let (_, matchSymbol, To nextState) as transition = step errorState transitionTable currentState nextSymbol
                let isEpsilonTransition = matchSymbol = OnEpsilon
                let nextInput = if isEpsilonTransition then input else rest
                let getNextValue = fTransition position transition isEpsilonTransition nextSymbol currentState nextState

                if nextState = errorState then
                    match fError position nextSymbol currentState currentValue getNextValue with
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
                    inner (getNextValue currentValue) nextState (position + 1) nextInput

        input
        |> List.ofSeq
        |> inner initialValue startState 0
