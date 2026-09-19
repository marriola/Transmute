// Project:     Transmute.Desktop
// Module:      Font view model
// Copyright:   (c) 2026 Matt Arriola
// License:     MIT

using Avalonia.Media;
using CommunityToolkit.Mvvm.ComponentModel;
using System.Collections.ObjectModel;

namespace Transmute.Desktop.ViewModels
{
    internal partial class FontDialogViewModel : ViewModelBase
    {
        [ObservableProperty]
        public partial ObservableCollection<FontFamily> Fonts { get; set; }
    }
}
