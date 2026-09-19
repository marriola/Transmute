// Project:     Transmute.Desktop
// Module:      Help browser
// Copyright:   (c) 2026 Matt Arriola
// License:     MIT

using Avalonia.Controls;
using Avalonia.Controls.ApplicationLifetimes;
using Avalonia.Platform;
using Avalonia.Styling;
using Microsoft.Extensions.DependencyInjection;
using MsBox.Avalonia;
using MsBox.Avalonia.Enums;
using System;
using System.Collections.ObjectModel;
using System.IO;
using System.Linq;
using System.Text.Json;
using System.Threading.Tasks;
using Transmute.Desktop.Models.WebInterop;
using Transmute.Desktop.Services;
using Transmute.Desktop.ViewModels;
using Transmute.Desktop.Views;

namespace Transmute.Desktop;

public partial class HelpBrowser : Window
{
    public ObservableCollection<HelpSectionViewModel> Sections { get; private init; } = [];

    private readonly HelpService _helpService = App.Current.Services?.GetService<HelpService>()!;

    private readonly ConfigurationService _configuration = App.Current.Services?.GetService<ConfigurationService>()!;

    private readonly JsonSerializerOptions _jsonSerializerOptions = new() { TypeInfoResolver = AppJsonContext.Default };

    public HelpBrowser()
    {
        InitializeComponent();

        HelpContent.EnvironmentRequested += (sender, args) =>
        {
            if (args is WindowsWebView2EnvironmentRequestedEventArgs e)
            {
                e.UserDataFolder = _helpService.WebViewFolder;
            }
        };

        _configuration?.ThemeChange += Configuration_ThemeChanged;

        _helpService?.Sections?.ForEach(Sections.Add);
    }

    private void HelpBrowser_Opened(object? sender, EventArgs e)
    {
        SelectedSection.SelectedItem = Sections[0];
        HelpContent.Source = new Uri(GetSectionFullPath(Sections[0].Path));
    }

    private void HelpBrowser_Closed(object? sender, EventArgs e)
    {
        _configuration?.ThemeChange -= Configuration_ThemeChanged;
    }

    private async void Configuration_ThemeChanged(object sender, ThemeVariant themeVariant)
    {
        await SetTheme(themeVariant.Key.ToString());
    }

    private async void SelectedSection_SelectionChanged(object? sender, SelectionChangedEventArgs e)
    {
        if (SelectedSection.SelectedItem is HelpSectionViewModel section)
        {
            RevealNode(section);

            if (section.Path.Contains('#'))
            {
                var parts = section.Path.Split('#');
                var url = GetSectionFullPath(parts[0], parts[1]);

                await HelpContent.InvokeScript("disableHighlight()");
                HelpContent.Navigate(new Uri(url));
            }
            else
            {
                HelpContent.Source = new Uri(GetSectionFullPath(section.Path));
            }
        }
    }

    private void RevealNode(HelpSectionViewModel section)
    {
        while (section.Parent != null)
        {
            section.Parent.IsExpanded = true;
            section = section.Parent;
        }
    }

    private async void HelpContent_NavigationCompleted(object? sender, WebViewNavigationCompletedEventArgs e)
    {
        await SetTheme(_configuration.Configuration.Theme, initial: true);
        var requestPath = e.Request!.AbsoluteUri;

        if (requestPath.Contains('#'))
        {
            if (SelectHeading(requestPath))
            {
                return;
            }

            requestPath = requestPath[..requestPath.IndexOf('#')];
        }

        if (Sections.FirstOrDefault(s => requestPath.EndsWith(s.Path)) is HelpSectionViewModel entry
            && SelectedSection.SelectedItem != entry)
        {
            SelectedSection.SelectedItem = entry;
        }
    }

    private async void HelpContent_WebMessageReceived(object? sender, WebMessageReceivedEventArgs e)
    {
        switch (JsonSerializer.Deserialize<HelpAction>(e.Body!, _jsonSerializerOptions))
        {
            case HighlightHeadingAction hha:
                SelectHeading(hha.Href);
                break;

            case LoadSampleAction lsa:
                await LoadSample(lsa.LexiconFile, lsa.RulesFile);
                break;
        }
    }

    private async Task LoadSample(string lexiconPath, string rulesPath)
    {
        if (App.Current.ApplicationLifetime is not IClassicDesktopStyleApplicationLifetime desktop
            || desktop.MainWindow is not MainWindow mainWindow)
        {
            return;
        }

        lexiconPath = Path.Join(_configuration.InitialDirectory, "sample", "ipa", lexiconPath);
        rulesPath = Path.Join(_configuration.InitialDirectory, "sample", "ipa", rulesPath);

        if (!File.Exists(lexiconPath))
        {
            await MessageBoxManager.GetMessageBoxStandard("Transmute", $"Couldn't find {lexiconPath}", ButtonEnum.Ok, MsBox.Avalonia.Enums.Icon.Error).ShowWindowDialogAsync(this);
            return;
        }

        if (!File.Exists(rulesPath))
        {
            await MessageBoxManager.GetMessageBoxStandard("Transmute", $"Couldn't find {rulesPath}", ButtonEnum.Ok, MsBox.Avalonia.Enums.Icon.Error).ShowWindowDialogAsync(this);
            return;
        }

        await mainWindow.InputPane.OpenLexicon(lexiconPath);
        await mainWindow.RulesPane.OpenRules(rulesPath);
        mainWindow.Activate();
    }

    private bool SelectHeading(string hash)
    {
        foreach (var section in Sections)
        {
            if (walk(section))
            {
                return true;
            }
        }

        return false;

        bool walk(HelpSectionViewModel section)
        {
            if (hash.EndsWith(section.Path))
            {
                SelectedSection.SelectedItem = section;
                return true;
            }

            foreach (var child in section.Children!)
            {
                if (walk(child))
                {
                    return true;
                }
            }

            return false;
        }
    }

    private async Task SetTheme(string? theme, bool initial = false)
    {
        string command = string.IsNullOrWhiteSpace(theme) || theme == "Default"
            ? "setColorScheme()"
            : $"setColorScheme('{theme.ToLower()}')";

        if (initial)
        {
            command = $"""
                if ('setColorScheme' in window)
                    {command};
                else
                    document.addEventListener('DOMContentLoaded', () => {command})
                """;
        }

        try
        {
            await HelpContent.InvokeScript(command);
        }
        catch (Exception ex)
        {
            Console.WriteLine(ex.ToString());
        }
    }

    private string GetSectionFullPath(string url, string? fragment = null)
    {
        var fullPath = new Uri(Path.Combine(_helpService.HelpFolder, url)).AbsoluteUri;

        if (fragment != null)
        {
            fullPath += '#' + fragment;
        }

        return fullPath;
    }
}
