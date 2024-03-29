﻿// Project:     TransmuteLib
// Module:      Special
// Description: Special characters
// Copyright:   (c) 2023 Matt Arriola
// License:     MIT

namespace TransmuteLib

/// Special characters used by the state machine
type internal Special =
    static member START = '␂'
    static member END = '␃'
    static member JOINER = '\ufeff' //'\u200d'
