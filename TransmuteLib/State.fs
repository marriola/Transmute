namespace TransmuteLib

type StateType = Final | NonFinal
type Segment = EnvironmentSegment | TargetSegment

type State =
    | State of name: string * segment: Segment * stateType: StateType
    | MergedState of State list * segment: Segment
    with
        /// Returns the state's name.
        static member name = function
            | State (name, _, _) -> name
            | MergedState (states, _) ->
                states
                |> Seq.map State.name
                |> String.concat (string Special.JOINER)

        /// Returns the ordinal part of a state's name (e.g. "q5" -> 5), -1 if the state name contains no ordinal part,
        /// or throws an exception if given a merged state.
        static member ord = function
            | State (name, _, _) ->
                match name.[1..] with
                | "" -> -1
                | x -> int x
            | MergedState _ ->
                failwith "Merged states have no ordinal"

        /// Creates a non-final state marked as matching input in the target segment.
        static member make name = State (name, TargetSegment, NonFinal)
    
        /// Marks a state as being final.
        static member makeFinal = function
            | State (name, isTarget, _) ->
                State (name, isTarget, Final)
            | MergedState _ as state ->
                failwithf "%s is a merged state; it cannot be made final" (string state)

        /// Marks a state as corresponding to input matched in the environment segment.
        static member makeEnvironment = function
            | State (name, _, isFinal) ->
                State (name, EnvironmentSegment, isFinal)
            | MergedState (states, _) ->
                let states =
                    states
                    |> List.map (fun (State (name, _, isFinal)) -> State (name, EnvironmentSegment, isFinal))
                MergedState (states, EnvironmentSegment)

        /// Merges a list of states into one merged state.
        static member merge states =
            let statesToMerge =
                states
                |> Seq.collect (function
                    | State _ as state -> [ state ]
                    | MergedState (states, _) -> states)
                |> Seq.distinct
                |> Seq.sortBy State.ord
                |> List.ofSeq
            let segment =
                match statesToMerge with
                | (State (_, TargetSegment, _))::_ ->
                    TargetSegment
                | _ ->
                    EnvironmentSegment
            match statesToMerge with
            | [state] -> state
            | _ -> MergedState (statesToMerge, segment)

        /// Returns a boolean indicating whether the state is final.
        static member isFinal = function
            | State (_, _, Final) -> true
            | State (_, _, NonFinal) -> false
            | MergedState (states, _) ->
                List.exists State.isFinal states

        /// Returns a boolean indicating whether the state corresponds to input matched in the environment segment.
        static member isEnvironment = function
            | State (_, EnvironmentSegment, _)
            | MergedState (_, EnvironmentSegment) ->
                true
            | _ ->
                false

        override this.ToString() =
            if State.isFinal this
                then State.name this |> sprintf "(%s)"
                else State.name this |> sprintf "%s"

type Transformation = Transition<State> * string
