// Project:     TransmuteLib
// Module:      State
// Description: FSA state type
// Copyright:   (c) 2023 Matt Arriola
// License:     MIT

namespace TransmuteLib

type StateType = Final | NonFinal

type TransitionResult =
    /// Output the same symbol that was consumed.
    | OutputDefault

    /// Drops the last N output symbols and adds a new symbol.
    | ReplacesWith of count: int * output: string

    | InsertsBefore of output: string

    /// Adds the consumed symbol, followed by a new symbol.
    | InsertsAfter of output: string

    /// Drops the last N output symbols.
    | Deletes of count: int * text: string
with
    member this.Or other =
        match this, other with
        | OutputDefault, x
        | x, OutputDefault ->
            x
        | x, y when x <> y ->
            failwithf "Tried to OR two transition results: %O, %O" x y
        | _ ->
            this

type State =
    | State of name: string * stateType: StateType
    | MergedState of State list
    with
        /// Returns the state's name.
        static member name = function
            | State (name, _) -> name
            | MergedState states ->
                states
                |> Seq.map State.name
                |> String.concat ""

        /// Returns the ordinal part of a state's name (e.g. "q5" -> 5), -1 if the state name contains no ordinal part,
        /// or throws an exception if given a merged state.
        static member ord = function
            | State (name, _) ->
                if name.StartsWith "S" then -1 else int (name.Substring(1))
            | MergedState _ ->
                failwith "Merged states have no ordinal"

        /// Creates a non-final state marked as matching a symbol in the input section.
        static member make name = State (name, NonFinal)
    
        /// Marks a state as being final.
        static member makeFinal = function
            | State (name, _) ->
                State (name, Final)
            | MergedState _ as state ->
                failwithf "%s is a merged state; it cannot be made final" (string state)
        
        /// Merges a list of states into one merged state.
        static member merge states =
            let statesToMerge =
                states
                |> Seq.collect (function
                    | State _ as state -> [ state ]
                    | MergedState states -> states)
                |> Seq.distinct
                |> Seq.sortBy State.ord
                |> List.ofSeq
            match statesToMerge with
            | [state] -> state
            | _ -> MergedState statesToMerge

        static member isMerged = function
            | MergedState _ -> true
            | _ -> false

        /// Returns a boolean indicating whether the state is final.
        static member isFinal = function
            | State (_, Final) -> true
            | State (_, NonFinal) -> false
            | MergedState states ->
                List.exists State.isFinal states

        override this.ToString() =
            let leftParen, rightParen = if State.isFinal this then "(", ")" else "", ""
            leftParen + State.name this + rightParen
