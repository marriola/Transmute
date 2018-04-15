module StateMachine

open System.Collections.Generic

/// <summary>
/// Represents how the the input symbol is matched to a transition. 
/// </summary>
type Match =
    /// <summary>
    /// The input symbol must match c/
    /// </summary>
    | Char of char

    /// <summary>
    /// If no other transition applies, match the transition without consuming an input symbol.
    /// </summary>
    | Epsilon

    /// <summary>
    /// If no other transition applies, accept any input symbol.
    /// </summary>
    | Any

/// <summary>
/// Represents a transition table.
/// </summary>
/// <typeparam name="TState">The type of states in the state machine.</typeparam>
type TransitionTable<'TState> = Dictionary<'TState * Match, 'TState>

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

/// <summary>
/// Creates a transition table.
/// </summary>
/// <typeparam name="TState">The type of states in the state machine.</typeparam>
/// <param name="classes">The list of characters and transitions that can be taken from them.</param>
let createTransitionTable<'TState when 'TState : equality> (classes:CharStateTransition<'TState> list) =
    // Add a list of keys with a single value
    let addMany (table: TransitionTable<'TState>) (keys: ('TState * Match) list) (value:'TState) =
        let rec addManyInternal keys value =
            match keys with
            | x::xs ->
                let toState, on = x
                table.[(value, on)] <- toState
                addManyInternal xs value
            | [] ->
                ()
        in
            addManyInternal keys value

    let rec fillInternal (table: TransitionTable<'TState>) classes =
        match classes with
        | x::xs ->
            addMany table x.transitions x.state
            fillInternal table xs
        | [] ->
            table
    in
        fillInternal (new TransitionTable<'TState>()) classes

/// <summary>
/// Creates a group of transitions from a state.
/// </summary>
/// <param name="state">The state to transition from.</param>
/// <param name="transitions">The transitions that can be taken from the state.</param>
let transitionFrom state transitions =
    { state = state;
      transitions = transitions
    }

/// <summary>
/// Creates a transition to a state on an input symbol.
/// </summary>
/// <param name="c">The input symbol.</param>
/// <param name="state">The state to transition to.</param>
let on (c: char) state = (state, Char c)

/// <summary>
/// Creates a transition to a state on an input symbol.
/// </summary>
/// <param name="state">The state to transition to.</param>
/// <param name="c">The input symbol.</param>
let transitionTo state (c: char) = (state, Char c)

/// <summary>
/// Creates a transition to a state on any symbol not already matched.
/// </summary>
/// <param name="state">The state to transition to.</param>
let anyTo state = (state, Any)

/// <summary>
/// Creates a transition to a state that does not consume an input symbol.
/// </summary>
/// <param name="state">The state to transition to.</param>
let epsilonTo state = (state, Epsilon)

/// <summary>
/// Compute the result of transitioning from the current state on an input symbol.
/// </summary>
/// <param name="transitionTable">The transition table.</param>
/// <param name="currentState">The current state to transition from.</param>
/// <param name="inputSymbol">The input symbol to transition on.</param>
/// <param name="failState">The error state to transition to if no other transition can be taken.</param>
let step<'a> (transitionTable: TransitionTable<'a>) (currentState: 'a) (inputSymbol: char) (failState: 'a) =
    let transition = (currentState, Char inputSymbol)
    let transitionOnEpsilon = (currentState, Epsilon)
    let transitionOnAny = (currentState, Any)

    if transitionTable.ContainsKey(transition) then
        (Char inputSymbol, transitionTable.[transition])
    else if transitionTable.ContainsKey(transitionOnEpsilon) then
        (Epsilon, transitionTable.[transitionOnEpsilon])
    else if transitionTable.ContainsKey(transitionOnAny) then
        (Any, transitionTable.[transitionOnAny])
    else
        (Char inputSymbol, failState)

