// Project:     Transmute.Desktop
// Module:      Output pane view model
// Copyright:   (c) 2026 Matt Arriola
// License:     MIT

using Avalonia.Controls.Primitives;
using CommunityToolkit.Mvvm.ComponentModel;
using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;
using Transmute.Desktop.Models;
using Transmute.Engine;

namespace Transmute.Desktop.ViewModels
{
    public partial class OutputPaneViewModel : ViewModelBase
    {
        public int OutputLexiconTab
        {
            get;

            set
            {
                field = value;
                ShowOutputLexicon = value == 0;
                ShowLexiconChanges = value == 1;
            }
        }

        [ObservableProperty]
        public partial bool ShowOutputLexicon { get; set; } = true;

        [ObservableProperty]
        public partial bool ShowLexiconChanges { get; set; }

        [ObservableProperty]
        public partial string OutputLexicon { get; set; }

        [ObservableProperty]
        public partial Dictionary<string, string> LexiconChanges { get; set; }

        [ObservableProperty]
        public partial ObservableCollection<LexiconChange> CurrentLexiconChange { get; set; } = new();

        public ObservableCollection<LexiconChanges> LexiconChangeList { get; set; } = new();

        [ObservableProperty]
        public partial ScrollBarVisibility HorizontalScrollBarVisibility { get; set; }

        public void SwitchTab(int tabIndex)
        {
            OutputLexiconTab = tabIndex;
        }

        public void LoadOutputLexicon(TransformResult[] outputLexicon, int? commentColumn, List<string?>? comments)
        {
            Reset();

            OutputLexicon = string.Join(Environment.NewLine, outputLexicon.Select((r, i) =>
            {
                if (commentColumn == null || comments == null || string.IsNullOrEmpty(comments[i]))
                {
                    return r.nextWord;
                }
                else
                {
                    var combiningDiacriticDifference = CountCombiningDiacritics(r.nextWord) - CountCombiningDiacritics(r.original);
                    var comment = new string(' ', commentColumn.Value - r.nextWord.Length + combiningDiacriticDifference) + comments[i];
                    return r.nextWord + comment;
                }
            }));

            foreach (var r in outputLexicon)
            {
                LexiconChangeList.Add(new LexiconChanges
                {
                    Header = $"{r.original} → {r.nextWord}",
                    Changes = r.changes.ToArray()
                });
            }
        }

        public void Reset()
        {
            OutputLexiconTab = 0;
            OutputLexicon = string.Empty;
            LexiconChangeList.Clear();
            CurrentLexiconChange.Clear();
        }

        private int CountCombiningDiacritics(string word)
        {
            var count = 0;

            foreach (var c in word)
            {
                if (c >= '\u0300' && c <= '\u0341')
                {
                    count++;
                }
            }

            return count;
        }
    }
}
