// Project:     Transmute.Desktop
// Module:      Input pane
// Copyright:   (c) 2026 Matt Arriola
// License:     MIT

using Avalonia.Controls;
using Avalonia.Interactivity;
using Avalonia.Platform.Storage;
using Avalonia.Threading;
using Avalonia.VisualTree;
using Microsoft.Extensions.DependencyInjection;
using MsBox.Avalonia;
using MsBox.Avalonia.Enums;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using Transmute.Desktop.Services;
using Transmute.Desktop.ViewModels;

namespace Transmute.Desktop.Views;

public partial class InputPane : UserControl
{
    private readonly ConfigurationService _configurationService = App.Current.Services?.GetService<ConfigurationService>()!;

    private readonly List<FilePickerFileType> _lexiconFileTypes = [
        new FilePickerFileType("Text files") { Patterns = [ "*.txt" ] },
        new FilePickerFileType("Any") { Patterns = [ "*.*" ] }
    ];

    public InputPane()
    {
        InitializeComponent();
    }

    public InputPaneViewModel ViewModel => (InputPaneViewModel)DataContext;

    private MainWindow MainWindow => this.FindAncestorOfType<MainWindow>()!;

    public async Task NewLexicon()
    {
        ViewModel.OriginalInputLexicon = string.Empty;
        ViewModel.InputLexicon = string.Empty;
    }

    public async Task OpenLexicon()
    {
        var file = await MainWindow.StorageProvider.OpenFilePickerAsync(new FilePickerOpenOptions
        {
            Title = "Open Lexicon",
            AllowMultiple = false,
            FileTypeFilter = _lexiconFileTypes,
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
            await ViewModel.LoadLexicon(file[0].Path.LocalPath, await file[0].OpenReadAsync());

            Dispatcher.UIThread.Post(async () =>
            {
                InputLexicon.ScrollToLine(0);
                InputLexicon.CaretIndex = 0;
            });
        }
        catch (FileNotFoundException)
        {
            await MessageBoxManager.GetMessageBoxStandard("Transmute", $"File not found: {file[0].Path.LocalPath}", ButtonEnum.Ok, Icon.Error).ShowWindowDialogAsync(MainWindow);
        }
    }

    public async Task SaveLexicon() => await SaveLexicon(false);

    public async Task SaveLexiconAs() => await SaveLexicon(true);

    public async Task<bool> SaveLexicon(bool saveAs)
    {
        if (ViewModel.LexiconPath == null || saveAs)
        {
            var file = await MainWindow.StorageProvider.SaveFilePickerAsync(new FilePickerSaveOptions
            {
                Title = "Save Lexicon",
                FileTypeChoices = _lexiconFileTypes,
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
            ViewModel.LexiconPath = file.Path.LocalPath;
        }

        using (var stream = new StreamWriter(ViewModel.LexiconPath))
        {
            await stream.WriteAsync(ViewModel.InputLexicon);
        }

        ViewModel.IsLexiconDirty = false;
        return true;
    }


    /// <summary>
    /// Checks if the lexicon needs to be saved before quitting or loading another lexicon.
    /// </summary>
    /// <param name="storageProvider"></param>
    /// <returns>True if the user cancelled, false if the user chose to save or discard.</returns>
    public async Task<bool> CheckLexiconDirty()
    {
        if (!ViewModel.IsLexiconDirty)
        {
            return false;
        }

        switch (await MessageBoxManager.GetMessageBoxStandard("Transmute", "The lexicon has unsaved changes. Do you want to save your changes?", ButtonEnum.YesNoCancel).ShowWindowDialogAsync(MainWindow))
        {
            case ButtonResult.Yes:
                if (!await SaveLexicon(false))
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

    protected override async void OnLoaded(RoutedEventArgs e)
    {
        base.OnLoaded(e);

        if (Design.IsDesignMode)
        {
            return;
        }

        foreach (var path in Program.Arguments.Reverse())
        {
            if (path.ToLower().EndsWith(".sc"))
            {
                continue;
            }

            await OpenLexicon(path);
            break;
        }

        if (!_configurationService.IsNew)
        {
            if (_configurationService.Configuration.LexiconFontFamily != null)
            {
                InputLexicon.FontFamily = _configurationService.Configuration.LexiconFontFamily;
            }

            if (_configurationService.Configuration.LexiconFontSize > 0)
            {
                InputLexicon.FontSize = _configurationService.Configuration.LexiconFontSize;
            }
        }
    }

    internal async Task OpenLexicon(string path)
    {
        using Stream s = File.OpenRead(path);
        await ViewModel.LoadLexicon(path, s);
        InputLexicon.Focus();
        InputLexicon.CaretIndex = 0;
    }

    private void Lexicon_TextChanged(object? sender, TextChangedEventArgs e)
    {
        if (ViewModel.IsLoading)
        {
            ViewModel.IsLoading = false;
        }
        else
        {
            ViewModel.IsLexiconDirty = InputLexicon.Text != ViewModel.OriginalInputLexicon;
        }
    }
}