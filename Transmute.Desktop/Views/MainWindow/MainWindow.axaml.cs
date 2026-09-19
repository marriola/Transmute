// Project:     Transmute.Desktop
// Module:      Main window
// Copyright:   (c) 2026 Matt Arriola
// License:     MIT

using Avalonia;
using Avalonia.Controls;
using Avalonia.Controls.Primitives;
using Avalonia.Interactivity;
using Avalonia.Media;
using Avalonia.Styling;
using Avalonia.VisualTree;
using Microsoft.Extensions.DependencyInjection;
using MsBox.Avalonia;
using MsBox.Avalonia.Dto;
using MsBox.Avalonia.Models;
using System;
using System.Diagnostics;
using System.Linq;
using System.Runtime.InteropServices;
using System.Threading.Tasks;
using Transmute.Desktop.Extensions;
using Transmute.Desktop.Models;
using Transmute.Desktop.Services;
using Transmute.Desktop.ViewModels;

namespace Transmute.Desktop.Views
{
    public partial class MainWindow : Window
    {
        private readonly ConfigurationService _configurationService = App.Current.Services?.GetService<ConfigurationService>()!;

        /// <summary>
        /// HACK: The menu's GettingFocus event fires three times. When the user uses one of the Alt hotkeys to jump to one of the sections,
        /// each event needs to be cancelled to keep the focus from jumping to the menu.
        /// </summary>
        private int didSelectTab = 0;

        private ScrollViewer inputScroll = null;
        private ScrollViewer outputScroll = null;

        private HelpBrowser? helpBrowser = null;

        private Size _previousSize;

        public MainWindow()
        {
            InitializeComponent();
        }

        internal bool ShowNativeMenu => RuntimeInformation.IsOSPlatform(OSPlatform.OSX);

        private MainWindowViewModel ViewModel => DataContext as MainWindowViewModel;

        public async Task ApplyRules()
        {
            RulesPane.StopNavigationListUpdate();
            
            if (await ViewModel.ApplyRules())
            {
                return;
            }

            RulesPane.GoToError();
        }

        public async Task Exit()
        {
            Close();
        }

        public void Cut() => RulesPane.Rules.Cut();

        public void Copy() => RulesPane.Rules.Copy();

        public void Paste() => RulesPane.Rules.Paste();

        public void OpenHelp()
        {
            if (helpBrowser != null)
            {
                helpBrowser.Activate();
                return;
            }

            helpBrowser = new();
            helpBrowser.Show();
            helpBrowser.Closed += (_, _) => helpBrowser = null;
        }

        public void SelectRuleTab(string tabIndexString)
        {
            didSelectTab = 2;
            RulesPane.SelectRuleTab(tabIndexString);
        }

        public void SelectInputLexicon()
        {
            didSelectTab = 2;
            InputPane.InputLexicon.Focus();
        }

        public void SelectOutputTab(string tabIndexString)
        {
            didSelectTab = 2;
            OutputPane.SelectOutputTab(tabIndexString);
        }

        public async Task PromptLine()
        {
            await RulesPane.PromptLine();
        }

        public void GoToChanges()
        {
            if (RulesPane.Rules.IsFocused)
            {
                return;
            }
            else if (_focusedEditor == InputPane.InputLexicon)
            {
                OutputPane.GoToChanges(InputPane.InputLexicon.LineNumber - 1);
            }
            else if (_focusedEditor == OutputPane.OutputLexicon)
            {
                OutputPane.GoToChanges(OutputPane.OutputLexicon.LineNumber - 1);
            }
        }

        public void GoToLine(int lineNumber, bool showRulesTab = true)
        {
            var entry = ViewModel.RulesPaneViewModel.TransitionTables.FirstOrDefault(t => t.lineNumber == lineNumber);

            if (entry != null)
            {
                RulesPane.SelectTransitionTable(entry);
            }

            RulesPane.GoToLine(lineNumber, showRulesTab);
        }

        public void GoBack()
        {
            RulesPane.NavigateBack();
        }

        public void GoForward()
        {
            RulesPane.NavigateForward();
        }

        public async Task NewRules()
        {
            if (await RulesPane.CheckRulesDirty())
            {
                return;
            }

            await RulesPane.NewRules();
        }

        public async Task OpenRules()
        {
            if (await RulesPane.CheckRulesDirty())
            {
                return;
            }

            await RulesPane.OpenRules();
        }

        public async Task SaveRules()
        {
            await RulesPane.SaveRules();
        }

        public async Task SaveRulesAs()
        {
            await RulesPane.SaveRulesAs();
        }

        public async Task NewLexicon()
        {
            if (await InputPane.CheckLexiconDirty())
            {
                return;
            }

            await InputPane.NewLexicon();
            OutputPane.NewLexicon();
        }

        public async Task OpenLexicon()
        {
            if (await InputPane.CheckLexiconDirty())
            {
                return;
            }

            await InputPane.OpenLexicon();
            OutputPane.ViewModel.Reset();
        }

        public async Task SaveLexicon()
        {
            await InputPane.SaveLexicon();
        }

        public async Task SaveLexiconAs()
        {
            await InputPane.SaveLexiconAs();
        }

        public async Task OpenFonts()
        {
            var win = new FontDialog();

            win.RulesFont.SelectedValue = RulesPane.Rules.FontFamily;
            win.RulesFontSize.SelectedValue = (int)RulesPane.Rules.FontSize;
            win.LexiconFont.SelectedValue = InputPane.InputLexicon.FontFamily;
            win.LexiconFontSize.SelectedValue = (int)InputPane.InputLexicon.FontSize;
            win.GridFont.SelectedValue = RulesPane.TransitionTableGrid.FontFamily;
            win.GridFontSize.SelectedValue = RulesPane.TransitionTableGrid.FontSize;

            var (rulesFont, rulesFontSize, lexiconFont, lexiconFontSize, gridFont, gridFontSize) = await win.ShowDialog<(FontFamily, int, FontFamily, int, FontFamily, int)>(this);

            if (rulesFont != null)
            {
                RulesPane.Rules.FontFamily = rulesFont;
                RulesPane.Rules.FontSize = rulesFontSize;

                _configurationService.Configuration.RulesFontFamily = rulesFont.Name;
                _configurationService.Configuration.RulesFontSize = rulesFontSize;
            }

            if (lexiconFont != null)
            {
                InputPane.InputLexicon.FontFamily = lexiconFont;
                InputPane.InputLexicon.FontSize = lexiconFontSize;
                OutputPane.OutputLexicon.FontFamily = lexiconFont;
                OutputPane.OutputLexicon.FontSize = lexiconFontSize;

                _configurationService.Configuration.LexiconFontFamily = lexiconFont.Name;
                _configurationService.Configuration.LexiconFontSize = lexiconFontSize;
            }

            if (gridFont != null)
            {
                RulesPane.TransitionTableGrid.FontFamily = gridFont;
                RulesPane.TransitionTableGrid.FontSize = gridFontSize;
                OutputPane.LexiconChangesGrid.FontFamily = gridFont;
                OutputPane.LexiconChangesGrid.FontSize = gridFontSize;
                _configurationService.Configuration.GridFontFamily = gridFont.Name;
                _configurationService.Configuration.GridFontSize = gridFontSize;
            }
        }

        public async Task ToggleChangeMarker(ChangeMarker changeMarker)
        {
            _configurationService.Configuration.ChangeMarker = changeMarker;
        }

        public async Task ToggleTheme(string theme)
        {
            var themeVariant = theme switch
            {
                "Light" => ThemeVariant.Light,
                "Dark" => ThemeVariant.Dark,
                _ => ThemeVariant.Default
            };

            _configurationService.ChangeTheme(themeVariant);
        }

        private TextBox? _focusedEditor = null;

        protected override void OnLoaded(RoutedEventArgs e)
        {
            base.OnLoaded(e);

            if (Design.IsDesignMode)
            {
                return;
            }

            this.GetObservable(WindowStateProperty).Subscribe(ws =>
            {
                // SizeChanged fires before WindowState changes and wipes out the saved window size in the config when maximizing, so restore it here

                if (ws == WindowState.Maximized)
                {
                    _configurationService.Configuration.WindowWidth = (int)_previousSize.Width;
                    _configurationService.Configuration.WindowHeight = (int)_previousSize.Height;
                }
            });

            InputPane.InputLexicon.GetObservable(IsFocusedProperty).Subscribe(f => _focusedEditor = InputPane.InputLexicon);

            OutputPane.OutputLexicon.GetObservable(IsFocusedProperty).Subscribe(f => _focusedEditor = OutputPane.OutputLexicon);
            
            if (!_configurationService.IsNew)
            {
                _configurationService.ChangeTheme(_configurationService.Configuration.ThemeVariant);
                SetHorizontalScrollBarVisibility();

                switch (_configurationService.Configuration.ChangeMarker ?? ChangeMarker.UnderlineChangeMarker)
                {
                    case ChangeMarker.NoChangeMarker:
                        rdoNoChangeMarker.IsChecked = true;
                        break;

                    case ChangeMarker.UnderlineChangeMarker:
                        rdoUnderlineChangeMarker.IsChecked = true;
                        break;

                    case ChangeMarker.DoubleUnderlineChangeMarker:
                        rdoDoubleUnderlineChangeMarker.IsChecked = true;
                        break;

                    case ChangeMarker.CaretChangeMarker:
                        rdoCaretChangeMarker.IsChecked = true;
                        break;
                }
            }

            if ((inputScroll = InputPane?.InputLexicon?.GetVisualDescendants()?.OfType<ScrollViewer>()?.FirstOrDefault()!) is null)
            {
                return;
            }

            inputScroll.ScrollChanged += (sender, e) =>
            {
                outputScroll.Offset = inputScroll.Offset;
            };

            if ((outputScroll = OutputPane?.OutputLexicon.GetVisualDescendants().OfType<ScrollViewer>().FirstOrDefault()!) is null)
            {
                return;
            }

            outputScroll.ScrollChanged += (sender, e) =>
            {
                inputScroll.Offset = outputScroll.Offset;
            };

            RulesPane.Rules.Focus();
        }

        private void TheMenu_Opened(object? sender, RoutedEventArgs e)
        {
            if (didSelectTab > 0)
            {
                didSelectTab--;
                TheMenu.Close();
            }
        }

        private void TheMenu_GettingFocus(object? sender, Avalonia.Input.FocusChangingEventArgs e)
        {
            if (didSelectTab > 0)
            {
                e.TryCancel();
                TheMenu.Close();
            }
        }

        public void ToggleHorizontalScrollbar()
        {
            _configurationService.Configuration.ShowHorizontalScrollbar = !_configurationService.Configuration.ShowHorizontalScrollbar;
            SetHorizontalScrollBarVisibility();
        }

        public void ToggleWordWrap()
        {
            _configurationService.Configuration.WordWrap = !_configurationService.Configuration.WordWrap;
            RulesPane.SetWordWrap(_configurationService.Configuration.WordWrap);
        }

        private void SetHorizontalScrollBarVisibility()
        {
            ViewModel.HorizontalScrollBarVisibility = _configurationService.Configuration.ShowHorizontalScrollbar ? ScrollBarVisibility.Visible : ScrollBarVisibility.Hidden;
        }

        private async void Window_Closing(object? sender, WindowClosingEventArgs e)
        {
            if (Design.IsDesignMode || App.Current.IsShuttingDown)
            {
                return;
            }

            helpBrowser?.Close();

            _configurationService.Configuration.IsMaximized = WindowState == WindowState.Maximized;
            _configurationService.Configuration.WindowX = Position.X;
            _configurationService.Configuration.WindowY = Position.Y;
            _configurationService.Save();

            if (!ViewModel.RulesPaneViewModel.IsRulesDirty && !ViewModel.InputPaneViewModel.IsLexiconDirty)
            {
                App.Current.Shutdown();
                return;
            }

            e.Cancel = true;

            if (await RulesPane.CheckRulesDirty() || await InputPane.CheckLexiconDirty())
            {
                return;
            }

            e.Cancel = false;
            App.Current.Shutdown();
        }

        private void Window_SizeChanged(object? sender, SizeChangedEventArgs e)
        {
            _previousSize = e.PreviousSize;
            _configurationService.Configuration.WindowWidth = (int)Width;
            _configurationService.Configuration.WindowHeight = (int)Height;
            Debug.WriteLine($"size changed: {e.NewSize}");
        }

        private void Help_Clicked(object? sender, RoutedEventArgs e)
        {
            OpenHelp();
        }

        private void AboutTransmute_Clicked(object? sender, RoutedEventArgs e)
        {
            ShowAboutDialogWithURL("Transmute", $"Transmute v0.2\n{Help.License}", "https://github.com/marriola/Transmute");
        }

        private void AboutTransmuteNative_Clicked(object? sender, EventArgs e)
        {
            AboutTransmute_Clicked(this, null);
        }

        private void AboutAvalonia_Clicked(object? sender, RoutedEventArgs e)
        {
            ShowAboutDialogWithURL("Avalonia UI", $"Avalonia UI 12.1.2\n© 2026 AvaloniaUI OÜ", "https://avaloniaui.net/");
        }

        private void AboutAvaloniaNative_Clicked(object? sender, EventArgs e)
        {
            AboutAvalonia_Clicked(this, null);
        }

        private void ShowAboutDialogWithURL(string name, string message, string url)
        {
            MessageBoxManager.GetMessageBoxCustom(new MessageBoxCustomParams
            {
                ButtonDefinitions = [new ButtonDefinition { Name = "OK" }],
                ContentTitle = $"About {name}",
                ContentMessage = message,
                HyperLinkParams = new HyperLinkParams
                {
                    Text = url,
                    Action = () => Utilities.OpenWebPage(url)
                }
            }).ShowWindowDialogAsync(this);
        }
    }
}