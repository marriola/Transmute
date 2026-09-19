// Project:     Transmute.Desktop
// Module:      View model base
// Copyright:   (c) 2026 Matt Arriola
// License:     MIT

using Avalonia.Controls;
using CommunityToolkit.Mvvm.ComponentModel;
using Microsoft.Extensions.DependencyInjection;

namespace Transmute.Desktop.ViewModels
{
    public abstract class ViewModelBase : ObservableObject
    {
        public static T? GetService<T>()
        {
            return Design.IsDesignMode
                ? default
                : App.Current.Services.GetService<T>();
        }
    }
}
