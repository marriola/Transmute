// Project:     TransmuteLib
// Module:      SyllableBoundaryDetector
// Description: Finds the locations of syllable boundaries in a word.
// Copyright:   (c) 2023 Matt Arriola
// License:     MIT

namespace TransmuteLib

open TransmuteLib.StateMachine

type SyllableSegment =
    | Onset
    | Nucleus
    | Coda

module SyllableBoundaryDetector =
    type DetectorState =
        { segments: char list
          syllableBoundaries: int list
          lastOutputPosition: int }

    let private findSegmentLocations parts =
        let inline segmentOf i c =
            match c with
            | 'O' -> i, Onset
            | 'N' -> i, Nucleus
            | 'C' -> i, Coda

        let rec inner i last out parts =
            match last, parts with
            | _, [] ->
                out |> List.rev |> Map.ofList
            | None, c::xs when c <> ' ' ->
                inner (i + 1) (Some c) (segmentOf i c :: out) xs
            | Some c1, c2::xs when c2 <> ' ' && c1 <> c2 ->
                inner (i + 1) (Some c2) (segmentOf i c2 :: out) xs
            | _, c::xs ->
                inner (i + 1) (Some c) out xs

        inner -1 None [] ([ ' ' ] @ parts @ [ ' ' ])

    /// <summary>
    /// Uses a syllable rule to find the locations of syllable boundaries.
    /// </summary>
    /// <param name="verbose">If true, displays the state of the state machine at each step.</param>
    /// <param name="rule">The syllable rule to apply.</param>
    /// <param name="word">The word to process.</param>
    let get verbose rule word =
        // Debug output columns
        //  is valid transition
        //  position in word
        //  transition
        //  --------------------
        //  output buffer

        let transitions, transformations = rule

        if verbose then
            printfn "%s" (new System.String('-', 80))

        let { segments = segments; syllableBoundaries = syllableBoundaries } =
            stateMachineConfig()
            |> withTransitions transitions
            |> withStartState (State.make "S_onset")
            |> withErrorState RuleCompiler.ERROR
            |> withInitialValue
                { segments = []
                  syllableBoundaries = [ 0 ]
                  lastOutputPosition = -1 }
            |> onError (fun input machineState ->
                let { currentState = current; currentValue = value; position = position} = machineState

                let nextSyllableBoundaries =
                    if position > value.lastOutputPosition then
                        position :: value.syllableBoundaries
                    else
                        value.syllableBoundaries

                if verbose then
                    let nextOutputStr = value.segments |> List.rev |> List.map string |> String.concat ""
                    printfn $"   %2d{position}   %c{input}: Error at %-25O{current} | %-20s{nextOutputStr}"

                Restart { value with syllableBoundaries = nextSyllableBoundaries })
            |> onTransition (fun symbol ((_, input, To nextState) as transition) machineState ->
                let isEpsilonTransition = input = OnEpsilon
                let { currentState = current; currentValue = value; position = position} = machineState
                let { segments = output; lastOutputPosition = lastOutputPosition } = value

                let nextOutputPosition, nextOutput =
                    if isEpsilonTransition then //|| position = lastOutputPosition then
                        lastOutputPosition, output
                    else
                        match Map.tryFind transition transformations with
                        | Some c -> position, c :: output
                        | _ -> lastOutputPosition, output

                if verbose then
                    let transition = $"{current} -> {nextState}"
                    let nextOutputStr = nextOutput |> List.rev |> List.map string |> String.concat ""
                    let e = if isEpsilonTransition then 'E' else ' '
                    printfn $" • %2d{position} %c{e} %c{symbol}: %-34s{transition} | %-20s{nextOutputStr}"

                { value with segments = nextOutput; lastOutputPosition = nextOutputPosition })
            |> onFinish id
            |> runNFA word

        let segmentLocations = segments |> List.rev |> findSegmentLocations
        let syllableBoundaries = syllableBoundaries

        //let s = String.Join("", output)
        //printfn "%s\n%s\n\n" word s
        //printfn "%A" boundaries

        syllableBoundaries, segmentLocations
