// Project:     Transmute.Desktop
// Module:      Error pane view model
// Copyright:   (c) 2026 Matt Arriola
// License:     MIT

using CommunityToolkit.Mvvm.ComponentModel;

namespace Transmute.Desktop.ViewModels
{
    public partial class ErrorPaneViewModel : ViewModelBase
    {
        public int? ErrorsTab
        {
            get;

            set
            {
                field = value;

                if (value is null)
                {
                    ShowErrors = false;
                    ShowWarnings = false;
                }
                else
                {
                    ShowErrors = value == 0;
                    ShowWarnings = value == 1;
                }
            }
        }

        [ObservableProperty]
        public partial bool ShowErrors { get; set; }

        [ObservableProperty]
        public partial bool ShowWarnings { get; set; }

        [ObservableProperty]
        public partial string? ErrorsTitle { get; set; } = "Errors (0)";

        [ObservableProperty]
        public partial string? WarningsTitle { get; set; } = "Warnings (0)";

        public int ErrorOffset { get; set; }
    }
}
