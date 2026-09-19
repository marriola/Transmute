// Project:     Transmute.Desktop
// Module:      Transmute JSON serializer context
// Copyright:   (c) 2026 Matt Arriola
// License:     MIT

using System.Collections.Generic;
using System.Text.Json.Serialization;
using Transmute.Desktop.Models.WebInterop;
using Transmute.Desktop.ViewModels;

namespace Transmute.Desktop
{
    [JsonSerializable(typeof(HelpSectionViewModel))]
    [JsonSerializable(typeof(List<HelpSectionViewModel>))]
    [JsonSerializable(typeof(HelpAction))]
    [JsonSerializable(typeof(HighlightHeadingAction))]
    [JsonSerializable(typeof(LoadSampleAction))]
    internal partial class AppJsonContext : JsonSerializerContext { }
}
