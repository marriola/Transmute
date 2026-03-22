// Project:     TransmuteLib
// Module:      Special
// Description: Special characters
// Copyright:   (c) 2023 Matt Arriola
// License:     MIT

namespace TransmuteLib

/// Special characters used by the state machine
module private Special =
    [<Literal>]
    let WORD_START_BOUNDARY = '␂'

    [<Literal>]
    let WORD_END_BOUNDARY = '␃'

    [<Literal>]
    let SYLLABLE_START_BOUNDARY = '₀'

    [<Literal>]
    let ONSET_START_BOUNDARY = '₁'

    [<Literal>]
    let ONSET_END_BOUNDARY = '₂'

    [<Literal>]
    let NUCLEUS_START_BOUNDARY = '₃'

    [<Literal>]
    let NUCLEUS_END_BOUNDARY = '₄'

    [<Literal>]
    let CODA_START_BOUNDARY = '₅'

    [<Literal>]
    let CODA_END_BOUNDARY = '₆'

    [<Literal>]
    let SYLLABLE_END_BOUNDARY = '₇'

    let WordBoundarySymbols = Set.ofList [
        WORD_START_BOUNDARY
        WORD_END_BOUNDARY
    ]

    let SyllableBoundarySymbols = Set.ofList [
        SYLLABLE_START_BOUNDARY
        SYLLABLE_END_BOUNDARY
        ONSET_START_BOUNDARY
        ONSET_END_BOUNDARY
        NUCLEUS_START_BOUNDARY
        NUCLEUS_END_BOUNDARY
        CODA_START_BOUNDARY
        CODA_END_BOUNDARY
    ]

    let Symbols = Set.unionMany [
        WordBoundarySymbols
        SyllableBoundarySymbols
    ]
