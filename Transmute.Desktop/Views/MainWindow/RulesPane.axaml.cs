// Project:     Transmute.Desktop
// Module:      Rules pane
// Copyright:   (c) 2026 Matt Arriola
// License:     MIT

using Avalonia.Controls;
using Avalonia.Input;
using Avalonia.Interactivity;
using Avalonia.Platform.Storage;
using Avalonia.Threading;
using Avalonia.VisualTree;
using Microsoft.Extensions.DependencyInjection;
using MsBox.Avalonia;
using MsBox.Avalonia.Enums;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;
using System.Threading.Tasks;
using Transmute.Desktop.Extensions;
using Transmute.Desktop.Models;
using Transmute.Desktop.Services;
using Transmute.Desktop.ViewModels;

namespace Transmute.Desktop.Views;

public partial class RulesPane : UserControl
{

    [GeneratedRegex(@"line (?<line>\d+), column (?<column>\d+)( \(offset (?<offset>\d+)\))?")]
    private static partial Regex LineColumnOffsetRegex();

    private static readonly Regex RE_ERROR_POSITION = LineColumnOffsetRegex();

    private readonly ConfigurationService _configurationService = App.Current.Services?.GetService<ConfigurationService>()!;

    private bool _dontUpdateNavigationStack = false;

    private readonly ErrorService _errorService = App.Current.Services?.GetService<ErrorService>()!;

    private readonly List<FilePickerFileType> _ruleFileTypes = [
        new FilePickerFileType("Sound change rule files") { Patterns = [ "*.sc" ] },
        new FilePickerFileType("Any") { Patterns = [ "*.*" ] }
    ];

    private readonly DispatcherTimer _navigationListUpdateTimer = new() { Interval = TimeSpan.FromMilliseconds(1000) };

    public RulesPane()
    {
        InitializeComponent();

        Rules.Options.HighlightCurrentLine = true;
        Rules.Options.AllowScrollBelowDocument = false;
        Rules.Options.WordWrapIndentation = 4;
        Rules.Options.EnableImeSupport = false;

        if (_configurationService != null)
        {
            Rules.WordWrap = _configurationService.Configuration.WordWrap;
        }

        Rules.AddHandler(KeyDownEvent, Rules_KeyDown, RoutingStrategies.Tunnel);
        Rules.AddHandler(KeyUpEvent, Rules_KeyUp, RoutingStrategies.Tunnel);

        _navigationListUpdateTimer.Tick += UpdateNavigationList;
    }

    public RulesPaneViewModel ViewModel => (RulesPaneViewModel)DataContext;

    private MainWindow MainWindow => this.FindAncestorOfType<MainWindow>();

    public void StopNavigationListUpdate()
    {
        _navigationListUpdateTimer.Stop();
    }

    public void SetWordWrap(bool wordWrap)
    {
        Rules.WordWrap = wordWrap;
    }

    public void SelectRuleTab(string tabIndexString)
    {
        var tabIndex = int.Parse(tabIndexString);
        ViewModel.RulesTab = tabIndex;

        if (tabIndex == 0)
        {
            Rules.Focus();
        }
        else if (tabIndex == 1)
        {
            SelectedTransitionTable.Focus();
        }
    }

    public void GoToDestination(string destination)
    {
        TransitionTableGrid.SelectedItems.Clear();
        var destinations = ViewModel.CurrentTransitionTable.Where(t => t.Origin == destination);

        if (destinations.Any())
        {
            TransitionTableGrid.ScrollIntoView(destinations.Last(), null);

            foreach (var row in destinations)
            {
                TransitionTableGrid.SelectedItems.Add(row);
            }
        }
    }

    public async Task PromptLine()
    {
        var win = new GoToLineWindow();
        win.LineNumber.Minimum = 1;
        win.LineNumber.Maximum = Rules.LineCount;
        win.LineCount.Text = Rules.LineCount.ToString();

        if (await win.ShowDialog<int?>(MainWindow) is int lineNumber)
        {
            GoToLine(lineNumber);
        }
    }

    public void GoToLine(int lineNumber, bool showRulesTab = true)
    {
        Dispatcher.UIThread.Post(async () =>
        {
            if (showRulesTab)
            {
                ViewModel.RulesTab = 0;
            }

            if (ViewModel.RulesTab == 0)
            {
                Rules.Focus();
            }

            Rules.ScrollToLine(lineNumber + 1);
            await Rules.HighlightLine(lineNumber, false);
            ViewModel.NavigateTo(RulesPaneViewModel.GetOffset(Rules.Text, lineNumber));
            UpdateHighlightedRuleSelection(lineNumber);
        });
    }

    public void GoToError()
    {
        if (RE_ERROR_POSITION.Match(_errorService.Errors[0]) is var match)
        {
            var line = int.Parse(match.Groups["line"].Value);
            var column = int.Parse(match.Groups["column"].Value);
            var offset = match.Groups.ContainsKey("offset")
                ? int.Parse(match.Groups["offset"].Value)
                : LineToOffset(line) + column - 1;

            Rules.Focus();
            //await Rules.HighlightLine(line, _viewModel.RulesPaneViewModel.GoToHighlight);
            Rules.ScrollToLine(line);
            Rules.CaretOffset = offset;

            ViewModel.UpdateRulesPosition(Rules.CaretOffset);
        }
    }

    public void NavigateBack()
    {
        if (!ViewModel.CanGoBack)
        {
            return;
        }

        var (line, position) = ViewModel.NavigateBack();
        Rules.CaretOffset = position;
        Rules.ScrollToLine(line);
        UpdateHighlightedRuleSelection(line);
    }

    public void NavigateForward()
    {
        if (!ViewModel.CanGoForward)
        {
            return;
        }

        var (line, position) = ViewModel.NavigateForward();
        Rules.CaretOffset = position;
        Rules.ScrollToLine(line);
        UpdateHighlightedRuleSelection(line);
    }

    public async Task NewRules()
    {
        ViewModel.OriginalRules = string.Empty;
        Rules.Text = string.Empty;
    }

    public async Task OpenRules()
    {
        var file = await MainWindow.StorageProvider.OpenFilePickerAsync(new FilePickerOpenOptions
        {
            Title = "Open Rules",
            AllowMultiple = false,
            FileTypeFilter = _ruleFileTypes,
            SuggestedStartLocation = _configurationService.Configuration.CurrentDirectory != null
                ? await MainWindow.StorageProvider.TryGetFolderFromPathAsync(_configurationService.Configuration.CurrentDirectory)
                : null
        });

        if (file.Count == 0)
        {
            _configurationService.Configuration.CurrentDirectory = null;
            return;
        }

        _configurationService.Configuration.CurrentDirectory = Path.GetDirectoryName(file[0].Path.LocalPath);

        try
        {
            await ViewModel.LoadRules(file[0].Path.LocalPath, await file[0].OpenReadAsync());

            Dispatcher.UIThread.Post(async () =>
            {
                HighlightedRule.ScrollViewer?.ScrollToHome();
                SelectedTransitionTable.ScrollViewer?.ScrollToHome();
                Rules.ScrollToLine(0);
                Rules.CaretOffset = 0;
                UpdateHighlightedRuleSelection(1);
            });
        }
        catch (FileNotFoundException)
        {
            await MessageBoxManager.GetMessageBoxStandard("Transmute", $"File not found: {file[0].Path.LocalPath}", ButtonEnum.Ok, Icon.Error).ShowWindowDialogAsync(MainWindow);
        }
    }

    public async Task SaveRules() => await SaveRules(false);

    public async Task SaveRulesAs() => await SaveRules(true);

    public async Task<bool> SaveRules(bool saveAs)
    {
        var path = ViewModel.RulesPath;

        if (path == null || saveAs)
        {
            var file = await MainWindow.StorageProvider.SaveFilePickerAsync(new FilePickerSaveOptions
            {
                Title = "Save Rules",
                FileTypeChoices = _ruleFileTypes,
                SuggestedStartLocation = _configurationService.Configuration.CurrentDirectory != null
                    ? await MainWindow.StorageProvider.TryGetFolderFromPathAsync(_configurationService.Configuration.CurrentDirectory)
                    : null
            });

            if (file == null)
            {
                _configurationService.Configuration.CurrentDirectory = null;
                return false;
            }

            _configurationService.Configuration.CurrentDirectory = Path.GetDirectoryName(file.Path.LocalPath);
            path = file.Path.LocalPath;
        }

        using (var stream = new StreamWriter(path ?? ViewModel.RulesPath))
        {
            await stream.WriteAsync(Rules.Text);
        }

        ViewModel.IsRulesDirty = false;
        return true;
    }

    /// <summary>
    /// Checks if the rules need to be saved before quitting or loading another rule set.
    /// </summary>
    /// <param name="storageProvider"></param>
    /// <returns>True if the user cancelled, false if the user chose to save or discard.</returns>
    public async Task<bool> CheckRulesDirty()
    {
        if (!ViewModel.IsRulesDirty)
        {
            return false;
        }

        switch (await MessageBoxManager.GetMessageBoxStandard("Transmute", "The rules have unsaved changes. Do you want to save your changes?", ButtonEnum.YesNoCancel).ShowWindowDialogAsync(MainWindow))
        {
            case ButtonResult.Yes:
                if (!await SaveRules(false))
                {
                    return true;
                }

                return false;

            case ButtonResult.No:
                return false;

            default:
                return true;
        }
    }

    public void SelectTransitionTable(NavigationEntry entry)
    {
        if (entry != SelectedTransitionTable.SelectedItem)
        {
            SelectedTransitionTable.SelectedItem = entry;
        }

        var index = ViewModel.TransitionTables.IndexOf(entry);
        ViewModel.LoadTransitionTable(index);
        GoToDestination("S");
    }

    protected override async void OnLoaded(RoutedEventArgs e)
    {
        base.OnLoaded(e);

        if (Design.IsDesignMode)
        {
            return;
        }

        foreach (var path in Program.Arguments.Reverse())
        {
            if (!path.ToLower().EndsWith(".sc"))
            {
                continue;
            }

            await OpenRules(path);
            break;
        }

        if (!_configurationService.IsNew)
        {
            if (_configurationService.Configuration.RulesFontFamily != null)
            {
                Rules.FontFamily = _configurationService.Configuration.RulesFontFamily;
            }

            if (_configurationService.Configuration.RulesFontSize > 0)
            {
                Rules.FontSize = _configurationService.Configuration.RulesFontSize;
            }

            if (_configurationService.Configuration.GridFontFamily != null)
            {
                TransitionTableGrid.FontFamily = _configurationService.Configuration.GridFontFamily;
            }

            if (_configurationService.Configuration.GridFontSize > 0)
            {
                TransitionTableGrid.FontSize = _configurationService.Configuration.GridFontSize;
            }
        }
    }

    internal async Task OpenRules(string path)
    {
        using Stream s = File.OpenRead(path);
        await ViewModel.LoadRules(path, s);
        Rules.Focus();
        UpdateHighlightedRuleSelection(1);
        Rules.CaretOffset = 0;
    }

    private void Rules_Tapped(object? sender, TappedEventArgs e)
    {
        ViewModel.UpdateRulesPosition(Rules.CaretOffset);
        ViewModel.NavigateTo(Rules.CaretOffset);
        UpdateSelectedRule();
    }

    private void Rules_TextChanged(object? sender, EventArgs e)
    {
        if (ViewModel.IsLoading)
        {
            ViewModel.IsLoading = false;
        }
        else
        {
            ViewModel.IsRulesDirty = Rules.IsModified;
            ViewModel.IsRuleSetCompiled = false;
            _navigationListUpdateTimer.Stop();
            _navigationListUpdateTimer.Start();
        }
    }

    private void Rules_KeyUp(object? sender, KeyEventArgs e)
    {
        ViewModel.UpdateRulesPosition(Rules.CaretOffset);
        UpdateSelectedRule();
    }

    private void Rules_KeyDown(object? sender, KeyEventArgs e)
    {
        ViewModel.UpdateRulesPosition(Rules.CaretOffset);
        UpdateSelectedRule();
    }

    private void HighlightedRule_SelectionChanged(object? sender, SelectionChangedEventArgs e)
    {
        if (_dontUpdateNavigationStack)
        {
            _dontUpdateNavigationStack = false;
            return;
        }

        if (e?.AddedItems?.Count > 0 && e?.AddedItems?[0] is NavigationEntry entry)
        {
            GoToLine(entry.lineNumber);
        }
    }

    private void TransitionTable_SelectionChanged(object? sender, SelectionChangedEventArgs e)
    {
        if (e?.AddedItems?.Count > 0 && e?.AddedItems?[0] is NavigationEntry entry)
        {
            SelectTransitionTable(entry);
        }
    }

    private void UpdateNavigationList(object? sender, EventArgs e)
    {
        _navigationListUpdateTimer.Stop();
        if (!ViewModel.PopulateNavigationList(showErrors: false))
        {
            return;
        }

        var (currentLineNumber, _) = RulesPaneViewModel.GetLineAndColumn(Rules.Text, Rules.CaretOffset);
        var selectedItem = new int[] { 0, -1, -2, 1, 2 }
            .Select(i => ViewModel.NavigationList.FirstOrDefault(n => n.lineNumber == currentLineNumber + i))
            .FirstOrDefault();

        if (selectedItem != null && selectedItem != HighlightedRule.SelectedItem)
        {
            _dontUpdateNavigationStack = true;
            HighlightedRule.SelectedItem = selectedItem;
        }
    }

    public void UpdateHighlightedRuleSelection(int line)
    {
        if (ViewModel.NavigationList.FirstOrDefault(n => n.lineNumber == line) is NavigationEntry entry && entry != HighlightedRule.SelectedItem)
        {
            _dontUpdateNavigationStack = true;
            HighlightedRule.SelectedItem = entry;
        }
    }

    private void Rules_PointerPressed(object? sender, PointerPressedEventArgs e)
    {
        if (e.Properties.IsXButton1Pressed && ViewModel.CanGoBack)
        {
            NavigateBack();
        }
        else if (e.Properties.IsXButton2Pressed && ViewModel.CanGoForward)
        {
            NavigateForward();
        }
    }

    private void UpdateSelectedRule()
    {
        if (ViewModel.NavigationList.OrderBy(n => n.lineNumber).LastOrDefault(n => n.lineNumber <= Rules.LineNumber) is NavigationEntry entry
            && entry != HighlightedRule.SelectedItem)
        {
            _dontUpdateNavigationStack = true;
            HighlightedRule.SelectedItem = entry;
        }
    }

    private int LineToOffset(int lineNumber)
    {
        int index = -1;

        for (var i = 0; i < lineNumber - 1; i++)
        {
            index = Rules.Text.IndexOf('\n', index + 1);
        }

        return index;
    }

    private int OffsetToLine(int offset)
    {
        var currentLine = 1;

        for (var i = 0; i < offset; i++)
        {
            if (Rules.Text[i] == '\n')
            {
                currentLine++;
            }
        }

        return currentLine;
    }
}