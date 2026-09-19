// Project:     Transmute.Desktop
// Module:      Help action: highlight section in table of contents
// Copyright:   (c) 2026 Matt Arriola
// License:     MIT

using System.Text.Json.Serialization;

namespace Transmute.Desktop.Models.WebInterop
{
    public class HighlightHeadingAction : HelpAction
    {
        [JsonPropertyName("href")]
        public required string Href { get; set; }
    }
}
