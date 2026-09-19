// Project:     Transmute.Desktop
// Module:      Error pane
// Copyright:   (c) 2026 Matt Arriola
// License:     MIT

using Avalonia.Controls;
using Avalonia.Input;
using Microsoft.Extensions.DependencyInjection;
using System.Collections.Generic;
using System.Linq;
using Transmute.Desktop.Services;
using Transmute.Desktop.ViewModels;

namespace Transmute.Desktop.Views;

public partial class ErrorPane : UserControl
{
    private readonly ErrorService _errorService = App.Current.Services?.GetService<ErrorService>()!;

    public ErrorPane()
    {
        InitializeComponent();

        _errorService?.Cleared += ErrorService_Cleared;
        _errorService?.ErrorsSet += ErrorService_ErrorsSet;
        _errorService?.WarningsSet += ErrorService_WarningsSet;
    }

    internal ErrorPaneViewModel ViewModel => (ErrorPaneViewModel)DataContext;

    private void ErrorService_Cleared()
    {
        Errors.Clear();
        Warnings.Clear();
        ViewModel.ErrorsTitle = "Errors (0)";
        ViewModel.WarningsTitle = "Warnings (0)";
    }

    private void ErrorService_ErrorsSet(IEnumerable<string> errors)
    {
        ViewModel.ErrorsTitle = $"Errors ({errors.Count()})";
        Errors.Text = string.Join('\n', errors);
    }

    private void ErrorService_WarningsSet(IEnumerable<string> warnings)
    {
        ViewModel.WarningsTitle = $"Warnings ({warnings.Count()})";
        Warnings.Text = string.Join('\n', warnings);
    }

    private void ErrorsTab_Tapped(object? sender, TappedEventArgs e)
    {
        ViewModel.ErrorsTab = ViewModel.ShowErrors ? null : 0;
    }

    private void WarningsTab_Tapped(object? sender, TappedEventArgs e)
    {
        ViewModel.ErrorsTab = ViewModel.ShowWarnings ? null : 1;
    }
}