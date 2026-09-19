// Project:     Transmute.Engine
// Module:      Special
// Description: Special characters
// Copyright:   (c) 2023 Matt Arriola
// License:     MIT

namespace Transmute.Engine

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

    let SymbolNames =
        [ WORD_START_BOUNDARY, "WordStart"
          SYLLABLE_START_BOUNDARY, "SyllableStart"
          ONSET_START_BOUNDARY, "OnsetStart"
          ONSET_END_BOUNDARY, "OnsetEnd"
          NUCLEUS_START_BOUNDARY, "NucleusStart"
          NUCLEUS_END_BOUNDARY, "NucleusEnd"
          CODA_START_BOUNDARY, "CodaStart"
          CODA_END_BOUNDARY, "CodaEnd"
          SYLLABLE_END_BOUNDARY, "SyllableEnd"
          WORD_END_BOUNDARY, "WordEnd" ]
        |> dict

    let Diacritics = set <| seq { '\u0300'..'\u0341' }
    let ToneDiacritics = set "\u0300\u0301\u0302\u0304\u030b\u030c\u030f"

    let Symbols = Set.unionMany [
        WordBoundarySymbols
        SyllableBoundarySymbols
    ]

    /// Wraps a word with word start and word end boundary symbols.
    let wrapWord word = string WORD_START_BOUNDARY + word + string WORD_END_BOUNDARY

    /// Removes word start and end boundary symbols from a word.
    let unwrapWord (word: string) = word.Replace(string WORD_START_BOUNDARY, "").Replace(string WORD_END_BOUNDARY, "")
