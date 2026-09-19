// Project:     Transmute.Desktop
// Module:      Help action: load sample
// Copyright:   (c) 2026 Matt Arriola
// License:     MIT

using System.Text.Json.Serialization;

namespace Transmute.Desktop.Models.WebInterop
{
    internal class LoadSampleAction : HelpAction
    {
        [JsonPropertyName("lexiconFile")]
        public string LexiconFile { get; set; }

        [JsonPropertyName("rulesFile")]
        public string RulesFile { get; set; }
    }
}
