// Project:     Transmute.Desktop
// Module:      Rules pane view model
// Copyright:   (c) 2026 Matt Arriola
// License:     MIT

using AvaloniaEdit.Document;
using CommunityToolkit.Mvvm.ComponentModel;
using Microsoft.FSharp.Collections;
using System;
using System.Collections.ObjectModel;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;
using System.Threading.Tasks;
using Transmute.Desktop.Models;
using Transmute.Desktop.Services;
using Transmute.Engine;
using static Transmute.Engine.Node;

namespace Transmute.Desktop.ViewModels
{
    public partial class RulesPaneViewModel : ViewModelBase
    {
        [GeneratedRegex(@"\$\w+\s*=\s*\(")]
        private static partial Regex XsampaRegex();

        private static readonly Regex RE_X_SAMPA = XsampaRegex();

        public ErrorPaneViewModel ErrorPaneViewModel { get; set; } = new();

        private ErrorService _errorService = GetService<ErrorService>()!;

        public int RulesTab
        {
            get;

            set
            {
                field = value;
                ShowRules = value == 0;
                ShowTransitionTables = value == 1;
            }
        }

        [ObservableProperty]
        public partial bool ShowRules { get; set; } = true;

        [ObservableProperty]
        public partial bool ShowTransitionTables { get; set; }

        [ObservableProperty]
        public partial string RulesPath { get; set; }

        [ObservableProperty]
        public partial bool IsCompiling { get; set; }

        [ObservableProperty]
        public partial string RulesPosition { get; set; } = "Line 1, column 1";

        public string OriginalRules { get; set; } = "";

        [ObservableProperty]
        public partial TextDocument Rules { get; set; } = new();

        [ObservableProperty]
        public partial int CurrentLine { get; set; }

        [ObservableProperty]
        public partial ObservableCollection<NavigationEntry> TransitionTables { get; set; } = new();

        [ObservableProperty]
        public partial ObservableCollection<NavigationEntry> NavigationList { get; set; } = new();

        [ObservableProperty]
        public partial ObservableCollection<TransitionModel> CurrentTransitionTable { get; set; } = new();

        [ObservableProperty]
        public partial string Status { get; set; }

        [ObservableProperty]
        public partial InputFormat InputFormat { get; set; } = InputFormat.IPA;

        public ObservableCollection<RuleLocation> RuleLocations { get; set; } = new();

        [ObservableProperty]
        public partial bool IsRulesDirty { get; set; }

        [ObservableProperty]
        public partial bool IsRuleSetCompiled { get; set; } = false;

        public RulesFile? CompiledRules { get; set; }

        [ObservableProperty]
        public partial ObservableCollection<NavigationHistory> NavigationHistory { get; set; } = new();

        [ObservableProperty]
        public partial NavigationHistory CurrentPosition { get; set; }

        public int NavStackPosition { get; set; } = 0;

        [ObservableProperty]
        public partial bool HasHistory { get; set; }

        [ObservableProperty]
        public partial bool CanGoBack { get; set; } = false;

        [ObservableProperty]
        public partial bool CanGoForward { get; set; } = false;

        [ObservableProperty]
        public partial bool IpaFix { get; set; } = true;

        public bool IsLoading { get; set; }

        public NavigationHistory NavigateBack()
        {
            if (NavStackPosition == NavigationHistory.Count - 1)
            {
                return NavigationHistory.Last();
            }

            CurrentPosition = NavigationHistory[++NavStackPosition];
            CanGoBack = NavStackPosition < NavigationHistory.Count - 1;
            CanGoForward = true;

            return CurrentPosition;
        }

        public NavigationHistory NavigateForward()
        {
            if (NavStackPosition == 0)
            {
                return NavigationHistory.First();
            }

            CurrentPosition = NavigationHistory[--NavStackPosition];
            CanGoBack = true;
            CanGoForward = NavStackPosition > 0;

            return CurrentPosition;
        }

        public void SelectHistory(NavigationHistory entry)
        {
            NavStackPosition = NavigationHistory.Index().FirstOrDefault(x => x.Item == entry).Index;
            CanGoBack = NavStackPosition < NavigationHistory.Count - 1;
            CanGoForward = NavStackPosition > 0;
        }

        public void NavigateTo(int position, string? desc)
        {
            var (line, _) = GetLineAndColumn(Rules.Text, position);

            for (var i = 0; i < NavStackPosition; i++)
            {
                NavigationHistory.RemoveAt(0);
            }

            CurrentPosition = new NavigationHistory(line, position, desc ?? line.ToString());
            NavigationHistory.Insert(0, CurrentPosition);
            NavStackPosition = 0;
            CanGoBack = NavigationHistory.Count > 1;
            CanGoForward = false;
            HasHistory = true;
        }

        public static int GetOffset(string text, int line)
        {
            var offset = 0;
            var current = 1;

            for (; offset < text.Length && current < line; offset++)
            {
                if (text[offset] == '\n')
                {
                    current++;
                }
            }

            return offset;
        }

        public static (int row, int col) GetLineAndColumn(string text, int offset)
        {
            var line = 1;
            var column = 1;

            for (var i = 0; i < offset; i++)
            {
                if (text[i] == '\n')
                {
                    line++;
                    column = 1;
                }
                else
                {
                    column++;
                }
            }

            return (line, column);
        }

        public void UpdateRulesPosition(int currentOffset)
        {
            if (Rules == null)
            {
                return;
            }

            var (row, col) = GetLineAndColumn(Rules.Text, currentOffset);
            RulesPosition = $"Line {row}, column {col}";
            CurrentLine = row;
        }

        public void Reset()
        {
            IsRulesDirty = false;
            IsRuleSetCompiled = false;
            RulesTab = 0;
            NavigationHistory.Clear();
            NavStackPosition = 0;
            CanGoForward = false;
            CanGoBack = false;
            HasHistory = false;
            Rules.Text = string.Empty;
            OriginalRules = string.Empty;
        }

        public async Task<bool> LoadRules(string path, Stream stream)
        {
            IsLoading = true;
            var text = await new StreamReader(stream).ReadToEndAsync();

            Reset();
            RulesPath = path;
            InputFormat = RE_X_SAMPA.IsMatch(text) ? InputFormat.X_SAMPA : InputFormat.IPA;
            OriginalRules = text;
            Rules.Text = text;

            TransitionTables.Clear();
            CurrentTransitionTable.Clear();
            
            return PopulateNavigationList();
        }

        public async Task<bool> CompileRules()
        {
            if (IsRuleSetCompiled || Rules == null)
            {
                return true;
            }

            IsCompiling = true;
            Status = "Compiling...";

            var text = Rules.Text;

            var (elapsedMs, result) = await Task.Run(async () =>
            {
                var options = RulesFileOptions.Default
                    .WithInputFormat(InputFormat)
                    .FromText(text)
                    .WithSilence(true);

                var stopwatch = new Stopwatch();

                RulesFile result;
                try
                {
                    stopwatch.Start();
                    result = await RulesFile.LoadAsync(options);
                    stopwatch.Stop();
                    return (stopwatch.ElapsedMilliseconds, result);
                }
                catch (Exception e)
                {
                    stopwatch.Stop();
                    var errorResult = new RulesFile(InputFormat, null, null, null, 0, false, false, FSharpList.Create([ e.ToString() ]));
                    return (stopwatch.ElapsedMilliseconds, errorResult);
                }
            });

            IsCompiling = false;
            _errorService.ClearErrors();

            if (!result.errors.IsEmpty)
            {
                _errorService.AddErrors(result.errors);
                ErrorPaneViewModel.ErrorsTab = 0;
                return false;
            }

            CompiledRules = result;
            PopulateTransitionTable();
            ErrorPaneViewModel.ErrorsTab = null;
            IsRuleSetCompiled = true;

            Status = $"Compiled {result.rules.Length} rules in {elapsedMs} ms";

            return true;
        }

        private void PopulateTransitionTable()
        {
            TransitionTables.Clear();
            CurrentTransitionTable.Clear();

            foreach (var rule in CompiledRules!.rules)
            {
                TransitionTables.Add(new NavigationEntry(rule.lineNumber, rule.node.ToString()));
            }
        }

        public bool PopulateNavigationList()
        {
            NavigationList.Clear();
            _errorService.ClearErrors();

            var result = RuleParser.Parse(InputFormat, Rules.Text);

            if (!result.errors.IsEmpty)
            {
                _errorService.AddErrors(result.errors);
                ErrorPaneViewModel.ErrorsTab = 0;
                return false;
            }

            var combinedRules = result.soundChangeRules
                .Concat(result.syllableRules)
                .OrderBy(NodeModule.getLine);

            foreach (var node in combinedRules)
            {
                var entry = node switch
                {
                    RuleNode => new NavigationEntry(NodeModule.getLine(node), node.ToString()),
                    SyllableDefinitionNode => new NavigationEntry(NodeModule.getLine(node), "Syllable definition"),
                    _ => throw new NotImplementedException($"Invalid rule node type {node.GetType()}")
                };

                NavigationList.Add(entry);
            }

            var setsAndFeatures = result.sets
                .Concat(result.features)
                .OrderBy(x => x.Key);

            foreach (var set in setsAndFeatures)
            {
                var lineNumber = set.Value switch
                {
                    SetDefinitionNode setDefinition => setDefinition.lineNumber,
                    FeatureDefinitionNode featureDefinition => featureDefinition.lineNumber,
                    _ => throw new NotImplementedException($"{set.Value.GetType()} is not supported")
                };

                NavigationList.Add(new NavigationEntry(lineNumber, set.Key, showLineNumber: false));
            }

            return true;
        }

        public void LoadTransitionTable(int index)
        {
            if (CompiledRules == null)
            {
                return;
            }

            var rule = CompiledRules.rules[index];
            var (transitions, transformations) = rule.compiledRule;

            CurrentTransitionTable.Clear();

            foreach (var kvp in transitions)
            {
                var transformation = MapModule.TryFind(Tuple.Create(Origin.NewFrom(kvp.Key.Item1), kvp.Key.Item2, Destination.NewTo(kvp.Value)), transformations);

                CurrentTransitionTable.Add(new TransitionModel
                {
                    Index = CurrentTransitionTable.Count + 1,
                    Origin = State.name.Invoke(kvp.Key.Item1),
                    Input = kvp.Key.Item2.ToString(),
                    Destination = State.name.Invoke(kvp.Value),
                    IsFinal = State.isFinal.Invoke(kvp.Value),
                    Output = transformation?.Value?.ToString()
                });
            }
        }
    }
}
