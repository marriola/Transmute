// Project:     Transmute.Desktop
// Module:      Output pane
// Copyright:   (c) 2026 Matt Arriola
// License:     MIT

using Avalonia.Controls;
using Avalonia.Interactivity;
using Microsoft.Extensions.DependencyInjection;
using Transmute.Desktop.Models;
using Transmute.Desktop.Services;
using Transmute.Desktop.ViewModels;

namespace Transmute.Desktop.Views;

public partial class OutputPane : UserControl
{
    private readonly ConfigurationService _configurationService = App.Current.Services?.GetService<ConfigurationService>()!;

    public OutputPane()
    {
        InitializeComponent();
    }

    public OutputPaneViewModel ViewModel => (OutputPaneViewModel)DataContext;

    public void SelectOutputTab(string tabIndexString)
    {
        var tabIndex = int.Parse(tabIndexString);
        ViewModel.OutputLexiconTab = tabIndex;

        if (tabIndex == 0)
        {
            OutputLexicon.Focus();
        }
        else if (tabIndex == 1)
        {
            SelectedOutputWord.Focus();
        }
    }

    public void NewLexicon()
    {
        ViewModel.OutputLexicon = string.Empty;
        ViewModel.LexiconChanges = new();
    }

    public void GoToDestination(int ruleNumber)
    {
        var current = Parent;

        while (current != null)
        {
            if (current is MainWindow mainWindow)
            {
                mainWindow.GoToLine(ruleNumber, showRulesTab: false);
                return;
            }

            current = current.Parent;
        }
    }

    public void GoToChanges(int index)
    {
        ViewModel.ShowLexiconChanges = true;
        SelectedOutputWord.SelectedIndex = index;
    }

    protected override void OnLoaded(RoutedEventArgs e)
    {
        base.OnLoaded(e);

        if (Design.IsDesignMode)
        {
            return;
        }
        
        if (!_configurationService.IsNew)
        {
            if (_configurationService.Configuration.LexiconFontFamily != null)
            {
                OutputLexicon.FontFamily = _configurationService.Configuration.LexiconFontFamily;
            }

            if (_configurationService.Configuration.LexiconFontSize > 0)
            {
                OutputLexicon.FontSize = _configurationService.Configuration.LexiconFontSize;
            }

            if (_configurationService.Configuration.GridFontFamily != null)
            {
                LexiconChangesGrid.FontFamily = _configurationService.Configuration.GridFontFamily;
            }

            if (_configurationService.Configuration.GridFontSize > 0)
            {
                LexiconChangesGrid.FontSize = _configurationService.Configuration.GridFontSize;
            }
        }
    }

    private async void ChangeList_SelectionChanged(object? sender, SelectionChangedEventArgs e)
    {
        if (e.AddedItems.Count == 0)
        {
            return;
        }

        var change = (LexiconChanges)e.AddedItems[0]!;

        ViewModel.CurrentLexiconChange.Clear();

        foreach (var c in change.Changes)
        {
            ViewModel.CurrentLexiconChange.Add(c);
        }
    }
}