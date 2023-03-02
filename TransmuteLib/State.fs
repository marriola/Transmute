namespace TransmuteLib

type StateType = Final | NonFinal
type Segment = EnvironmentSegment | InputSegment
type StateBehavior = Replace | Insert

type State =
    | State of name: string * segment: Segment * stateType: StateType * stateBehavior: StateBehavior
    | MergedState of State list * segment: Segment * stateBehavior: StateBehavior
    with
        /// Returns the state's name.
        static member name = function
            | State (name, _, _, _) -> name
            | MergedState (states, _, _) ->
                states
                |> Seq.map State.name
                |> String.concat (string Special.JOINER)

        /// Returns the ordinal part of a state's name (e.g. "q5" -> 5), -1 if the state name contains no ordinal part,
        /// or throws an exception if given a merged state.
        static member ord = function
            | State (name, _, _, _) ->
                match name.[1..] with
                | "" -> -1
                | x -> int x
            | MergedState _ ->
                failwith "Merged states have no ordinal"

        /// Creates a non-final state marked as matching a symbol in the input segment.
        static member make name = State (name, InputSegment, NonFinal, Replace)
    
        /// Marks a state as being final.
        static member makeFinal = function
            | State (name, isInput, _, behavior) ->
                State (name, isInput, Final, behavior)
            | MergedState _ as state ->
                failwithf "%s is a merged state; it cannot be made final" (string state)

        /// Marks a state as corresponding to input matched in the environment segment.
        static member makeEnvironment = function
            | State (name, _, isFinal, behavior) ->
                State (name, EnvironmentSegment, isFinal, behavior)
            | MergedState (states, _, behavior) ->
                let states =
                    List.map
                        (fun (State (name, _, isFinal, behavior)) -> State (name, EnvironmentSegment, isFinal, behavior))
                        states
                MergedState (states, EnvironmentSegment, behavior)

        static member makeInput = function
            | State (name, _, isFinal, behavior) ->
                State (name, InputSegment, isFinal, behavior)
            | MergedState (states, _, behavior) ->
                let states =
                    states
                    |> List.map (fun (State (name, _, isFinal, behavior)) -> State (name, InputSegment, isFinal, behavior))
                MergedState (states, InputSegment, behavior)
        
        static member makeInsert = function
            | State (name, segment, isFinal, _) ->
                State (name, segment, isFinal, Insert)
            | MergedState (states, segment, _) ->
                MergedState (states, segment, Insert)

        /// Merges a list of states into one merged state.
        static member merge states =
            let statesToMerge =
                states
                |> Seq.collect (function
                    | State _ as state -> [ state ]
                    | MergedState (states, _, _) -> states)
                |> Seq.distinct
                |> Seq.sortBy State.ord
                |> List.ofSeq
            let segment =
                match statesToMerge with
                | (State (_, InputSegment, _, _))::_ ->
                    InputSegment
                | _ ->
                    EnvironmentSegment
            let behavior =
                if List.forall (function
                    | State (_, _, _, behavior)
                    | MergedState (_, _, behavior) when behavior = Insert ->
                        true
                    | _ ->
                        false) statesToMerge
                    then Insert
                    else Replace
            match statesToMerge with
            | [state] -> state
            | _ -> MergedState (statesToMerge, segment, behavior)

        /// Returns a boolean indicating whether the state is final.
        static member isFinal = function
            | State (_, _, Final, _) -> true
            | State (_, _, NonFinal, _) -> false
            | MergedState (states, _, _) ->
                List.exists State.isFinal states

        /// Returns a boolean indicating whether the state corresponds to input matched in the environment segment.
        static member isEnvironment = function
            | State (_, EnvironmentSegment, _, _)
            | MergedState (_, EnvironmentSegment, _) ->
                true
            | _ ->
                false

        override this.ToString() =
            let leftParen, rightParen = if State.isFinal this then "(", ")" else "", ""
            let indicator = if State.isEnvironment this then "ᴱ" else "ᴵ"
            leftParen + State.name this + rightParen + indicator

type Transformation = Transition<State> * string
