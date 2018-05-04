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

type Edge<'TState> = 'TState * Match

type Transition<'TState> = Edge<'TState> * 'TState list

/// <summary>
/// Represents a transition table.
/// </summary>
/// <typeparam name="TState">The type of states in the state machine.</typeparam>
type TransitionTable<'TState> = IDictionary<'TState * Match, 'TState list>

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
        |> List.map
            (fun c -> c.transitions |> List.map (fun t -> (c.state, snd t), [ fst t ]))
        |> List.concat
        |> dict

    let groupTransitions transitions =
        transitions
        |> List.groupBy (fun t -> fst t)
        |> List.map (fun (key, lst) -> key, lst |> List.map snd |> List.concat)

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
        |> Seq.map (fun cset -> cset |> Seq.map (fun c -> (state, Char c)))
        |> Seq.concat
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
    /// <param name="failState">The error state to transition to if no other transition can be taken.</param>
    let step (transitionTable: TransitionTable<'a>) currentState inputSymbol failState =
        let transition = currentState, Char inputSymbol
        let transitionOnEpsilon = currentState, Epsilon
        let transitionOnAny = currentState, Any

        if transitionTable.ContainsKey(transition) then
            Char inputSymbol, transitionTable.[transition].[0]

        else if transitionTable.ContainsKey(transitionOnEpsilon) then
            Epsilon, transitionTable.[transitionOnEpsilon].[0]

        else if transitionTable.ContainsKey(transitionOnAny) then
            Any, transitionTable.[transitionOnAny].[0]

        else
            Char inputSymbol, failState

