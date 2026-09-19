// Project:     Transmute.Desktop
// Module:      Help action
// Copyright:   (c) 2026 Matt Arriola
// License:     MIT

using System.Text.Json.Serialization;

namespace Transmute.Desktop.Models.WebInterop
{
    [JsonPolymorphic(TypeDiscriminatorPropertyName = "type")]
    [JsonDerivedType(typeof(HighlightHeadingAction), typeDiscriminator: "highlightHeading")]
    [JsonDerivedType(typeof(LoadSampleAction), typeDiscriminator: "loadSample")]
    public abstract class HelpAction
    {
    }
}
