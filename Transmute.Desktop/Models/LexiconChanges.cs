// Project:     Transmute.Desktop
// Module:      Lexicon change model
// Copyright:   (c) 2026 Matt Arriola
// License:     MIT

using Transmute.Engine;

namespace Transmute.Desktop.Models
{
    public class LexiconChanges
    {
        public string Header { get; set; }

        public LexiconChange[] Changes { get; set; }
    }
}
