// Project:     TransmuteLib
// Module:      SyllableBoundaryDetector
// Description: Finds the locations of syllable boundaries in a word.
// Copyright:   (c) 2026 Matt Arriola
// License:     MIT

namespace TransmuteLib

module private SyllableBoundaryDetector =
    type private DetectorState =
        { segments: char list
          syllableBoundaries: int list
          lastOutputPosition: int }

    let private startSegments = dict [
        'O', OnsetStart
        'N', NucleusStart
        'C', CodaStart
    ]

    let private endSegments = dict [
        'O', OnsetEnd
        'N', NucleusEnd
        'C', CodaEnd
    ]

    let private findSegmentBoundaryLocations parts =
        let rec inner i last out parts =
            match last, parts with
            | _, [] ->
                List.rev out
            | None, c::[] when c = ' ' ->
                let nextOut = (i, SyllableEnd) :: out
                inner i None nextOut []
            | None, c::xs when c = ' ' ->
                let nextOut = (i, SyllableStart) :: out
                inner i None nextOut xs
            | None, c::xs when c <> ' ' ->
                let nextOut = (i, startSegments[c]) :: out
                inner (i + 1) (Some c) nextOut xs
            | Some c1, c2::xs when c1 <> c2 ->
                let nextOut =
                    if c2 > c1 then
                        (i, SyllableStart) :: (i, SyllableEnd) :: (i, endSegments[c1]) :: out
                    else
                        (i, endSegments[c1]) :: out
                inner i None nextOut parts
            | _, c::xs ->
                inner (i + 1) last out xs

        inner 0 None [] ([' '] @ parts @ [' '])

    let private insertSyllableBoundaries (boundaryTypes: Set<SyllableBoundaryType>) (word: string) (parts: (int * SyllableBoundaryType) list) =
        let word = Array.ofSeq word
        let _, wordWithBoundaries =
            ((0, word), parts)
            ||> List.fold (fun (offset, word) (i, part) ->
                if Set.contains part boundaryTypes then
                    offset + 1, Array.insertAt (i + offset) SyllableBoundaryType.BoundaryTypeToChar.[part] word
                else
                    offset, word)
        (new System.String(wordWithBoundaries))

    let private findSegmentLocations parts =
        let inline segmentOf i c =
            match c with
            | 'O' -> i, OnsetStart
            | 'N' -> i, NucleusStart
            | 'C' -> i, CodaStart

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

    let private allBoundaryTypes = Set.ofList [ SyllableStart; SyllableEnd; OnsetStart; OnsetEnd; NucleusStart; NucleusEnd; CodaStart; CodaEnd ]

    /// Classifies each segment in a word as belonging to either the onset, nucleus or coda.
    let private classifySegments (rule: string -> string) word =
        (rule word).Replace(".", "")
        |> Seq.toList

    let private isValidSyllableSegment = function
        | 'N' -> false
        | 'C' -> false
        | 'O' -> false
        | _ -> true

    /// Inserts syllable boundary markers into a word.
    let findBoundaries (rule: string -> string) (word: string) =
        let segments = classifySegments rule word

        if List.exists isValidSyllableSegment segments then
            // if the syllabizer returned any leftover segments, the syllable rule wasn't able to match the whole word
            let segments =
                segments
                |> List.map string
                |> String.concat ""
            Result.Error (sprintf "warning: failed to syllabize '%s' (syllabizer returned '%s')" word segments)
        else
            let segmentLocations = findSegmentLocations segments
        
            let segmentedWord =
                segments
                |> findSegmentBoundaryLocations
                |> insertSyllableBoundaries allBoundaryTypes word

            Result.Ok (segmentLocations, segmentedWord)
