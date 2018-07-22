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

    type ErrorAction<'TResult> =
        | Restart
        | Stop of 'TResult

    type Config<'TState, 'TValue, 'TResult when 'TState : equality> =
        { transitionTable: TransitionTable<'TState> option;
          startState:'TState option;
          errorState:'TState option;
          initialValue: 'TValue option;
          transitionFromStartOnFail: bool;
          fError: (('TValue -> 'TState -> int -> string -> 'TResult) -> int -> 'TValue -> ErrorAction<'TResult>) option;
          fTransition: (bool -> bool -> char -> 'TState -> 'TState -> 'TValue -> 'TValue) option;
          fAccept: ('TState -> bool) option;
          fFinish: ('TValue -> 'TResult) option
        }

    type CompleteConfig<'TState, 'TValue, 'TResult when 'TState : equality> =
        { transitionTable: TransitionTable<'TState>;
          startState:'TState;
          errorState:'TState;
          initialValue: 'TValue;
          transitionFromStartOnFail: bool;
          fError: (('TValue -> 'TState -> int -> string -> 'TResult) -> int -> 'TValue -> ErrorAction<'TResult>);
          fTransition: (bool -> bool -> char -> 'TState -> 'TState -> 'TValue -> 'TValue);
          fAccept: ('TState -> bool);
          fFinish: ('TValue -> 'TResult)
        }

    let stateMachineConfig<'TState, 'TValue, 'TResult when 'TState : equality> () : Config<'TState, 'TValue, 'TResult> =
        { transitionTable = None
          startState = None
          errorState = None
          initialValue = None
          transitionFromStartOnFail = false;
          fError = None;
          fTransition = None;
          fAccept = None;
          fFinish = None }

    let withTransitions table (config: Config<'TState, 'TValue, 'TResult>) =
        { config with transitionTable = Some table }

    let withStartState state (config: Config<'TState, 'TValue, 'TResult>) =
        { config with startState = Some state }

    let withErrorState state (config: Config<'TState, 'TValue, 'TResult>) =
        { config with errorState = Some state }

    let withInitialValue value (config: Config<'TState, 'TValue, 'TResult>) =
        { config with initialValue = Some value }

    let withTransitionFromStartOnFail (config: Config<'TState, 'TValue, 'TResult>) =
        { config with transitionFromStartOnFail = true }

    let onError fError (config: Config<'TState, 'TValue, 'TResult>) =
        { config with fError = Some fError }

    let onTransition fTransition (config: Config<'TState, 'TValue, 'TResult>) =
        { config with fTransition = Some fTransition }

    let onAccept fAccept (config: Config<'TState, 'TValue, 'TResult>) =
        { config with fAccept = Some fAccept }

    let onFinish fFinish (config: Config<'TState, 'TValue, 'TResult>) =
        { config with fFinish = Some fFinish }

    let private completeConfig (config: Config<'TState, 'TValue, 'TResult>) =
        if Option.isSome config.transitionTable
            || Option.isSome config.startState
            || Option.isSome config.errorState
            || Option.isSome config.initialValue
            || Option.isSome config.fError
            || Option.isSome config.fTransition
            || Option.isSome config.fAccept
            || Option.isSome config.fFinish then
            { transitionTable = Option.get config.transitionTable
              startState = Option.get config.startState
              errorState = Option.get config.errorState
              initialValue = Option.get config.initialValue
              transitionFromStartOnFail = config.transitionFromStartOnFail;
              fError = Option.get config.fError
              fTransition = Option.get config.fTransition
              fAccept = Option.get config.fAccept
              fFinish = Option.get config.fFinish }
        else
            failwith "Configuration is incomplete"

    /// <summary>
    /// 
    /// </summary>
    /// <param name="transitionTable">The transition table.</param>
    /// <param name="startState">The start state.</param>
    /// <param name="errorState">The error state.</param>
    /// <param name="initialValue">The initial value.</param>
    /// <param name="fError">Produces a value when the state machine fails.</param>
    /// <param name="fTransition">Produces the next value from a transition.</param>
    /// <param name="fAccept">Returns true if the state is a final state; otherwise, false.</param>
    /// <param name="fFinish">Produces the final return value of the state machine.</param>
    /// <param name="input">The input to iterate over.</param>
    let runStateMachine<'TState, 'TValue, 'TResult when 'TState : equality>
        input
        (config: Config<'TState, 'TValue, 'TResult>)
        : 'TResult =
        let { transitionTable=transitionTable;
              startState=startState;
              errorState=errorState;
              initialValue=initialValue;
              transitionFromStartOnFail=transitionFromStartOnFail;
              fError=fError;
              fTransition=fTransition;
              fAccept=fAccept;
              fFinish=fFinish
            } = completeConfig config

        let rec inner currentValue currentState inputPosition (input: string) =
            let step state nextSymbol =
                let rec stepInternal state transitioningFromStartOnFail =
                    let matchSymbol, next = step errorState transitionTable state nextSymbol
                    // If we can't step from the current state, and the current state is final, try stepping from START
                    if next = errorState && transitioningFromStartOnFail && fAccept state then
                        stepInternal startState false
                    else
                        matchSymbol, next
                stepInternal state transitionFromStartOnFail

            if inputPosition >= input.Length then
                fFinish currentValue
            else
                //let nextSymbol, xs = Seq.head input, Seq.tail input
                let nextSymbol = input.[inputPosition]
                let matchSymbol, nextState = step currentState nextSymbol
                let nextInputPosition = if matchSymbol = Epsilon then inputPosition else inputPosition + 1

                let isNextFinal = fAccept nextState
                let isEpsilon = matchSymbol = Epsilon
                let nextValue = fTransition isNextFinal isEpsilon nextSymbol currentState nextState currentValue

                if nextState = errorState then
                    match fError inner inputPosition nextValue with
                    | Restart when currentState <> startState ->
                        inner nextValue startState inputPosition input
                    | Restart when inputPosition < input.Length - 1 ->
                        inner nextValue startState nextInputPosition input
                    | Restart ->
                        fFinish currentValue
                    | Stop result ->
                        result
                else
                    inner nextValue nextState nextInputPosition input

        inner initialValue startState 0 input
