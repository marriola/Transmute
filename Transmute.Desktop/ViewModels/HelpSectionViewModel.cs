// Project:     Transmute.Desktop
// Module:      Help section view model
// Copyright:   (c) 2026 Matt Arriola
// License:     MIT

using CommunityToolkit.Mvvm.ComponentModel;
using System.Collections.Generic;
using System.Text.Json.Serialization;

namespace Transmute.Desktop.ViewModels
{
    public partial class HelpSectionViewModel : ViewModelBase
    {
        public string? Heading { get; set; }

        public required string Path { get; set; }

        public required List<HelpSectionViewModel>? Children { get; set; }

        [JsonIgnore]
        public HelpSectionViewModel? Parent { get; set; }

        [JsonIgnore]
        [ObservableProperty]
        public partial bool IsExpanded { get; set; }
    }
}
