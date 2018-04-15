module StateMachine

open System.Collections.Generic

// Represents how the the input symbol is matched to a transition.
//
// Char c   The input symbol must match c
// Epsilon  If no other transition applies, match the transition without consuming an input symbol
// Any      If no other transition applies, accept any input symbol

type Match =
    | Char of char
    | Epsilon
    | Any

type TransitionTable<'TState> = Dictionary<'TState * Match, 'TState>

type SymbolClass<'TState, 'TSymbol> =
    { state: 'TState;
      members: ('TState * 'TSymbol) list;
    }

type CharacterClass<'TState> = SymbolClass<'TState, Match>

let createTransitionTable<'TState when 'TState : equality> (classes:CharacterClass<'TState> list) =
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
            addMany table x.members x.state
            fillInternal table xs
        | [] ->
            table
    in
        fillInternal (new TransitionTable<'TState>()) classes

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

let transitionFrom state members =
    { state = state;
      members = members
    }

let on (c: char) state = (state, Char c)
let transitionTo state (c: char) = (state, Char c)
let anyTo state = (state, Any)
let epsilonTo state = (state, Epsilon)
