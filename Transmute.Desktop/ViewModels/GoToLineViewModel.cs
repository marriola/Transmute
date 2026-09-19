// Project:     Transmute.Desktop
// Module:      Go to line dialog view model
// Copyright:   (c) 2026 Matt Arriola
// License:     MIT

using CommunityToolkit.Mvvm.ComponentModel;

namespace Transmute.Desktop.ViewModels
{
    internal partial class GoToLineViewModel : ViewModelBase
    {
        [ObservableProperty]
        public partial int LineNumber { get; set; }

        [ObservableProperty]
        public partial int LineCount { get; set; }
    }
}
