// Project:     Transmute.Desktop
// Module:      Input pane view model
// Copyright:   (c) 2026 Matt Arriola
// License:     MIT

using Avalonia.Controls.Primitives;
using CommunityToolkit.Mvvm.ComponentModel;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using Transmute.Desktop.Services;
using Transmute.Engine;

namespace Transmute.Desktop.ViewModels
{
    public partial class InputPaneViewModel : ViewModelBase
    {
        [ObservableProperty]
        public partial string LexiconPath { get; set; }

        public string OriginalInputLexicon { get; set; }

        [ObservableProperty]
        public partial string? InputLexicon { get; set; }

        [ObservableProperty]
        public partial bool IsLexiconTransformed { get; set; }

        [ObservableProperty]
        public partial bool IsLexiconDirty { get; set; }

        [ObservableProperty]
        public partial string Status { get; set; }

        [ObservableProperty]
        public partial ScrollBarVisibility HorizontalScrollBarVisibility { get; set; }

        public bool IsLoading { get; set; }

        private ConfigurationService _configurationService = GetService<ConfigurationService>()!;

        private ErrorService _errorService = GetService<ErrorService>()!;

        public async Task LoadLexicon(string path, Stream stream)
        {
            IsLoading = true;
            OriginalInputLexicon = InputLexicon = await new StreamReader(stream).ReadToEndAsync();
            LexiconPath = path;
            IsLexiconDirty = false;
        }

        public async Task<(TransformResult[]? results, int? commentColumn, List<string?> comments)> ApplyRules(RulesFile compiledRules)
        {
            var comments = new List<string?>();
            var commentColumn = default(int?);

            if (InputLexicon == null)
            {
                return (null, commentColumn, comments);
            }

            var lexicon = InputLexicon.Split('\n').Select(w =>
            {
                var commentIndex = w.IndexOf(';');
                if (commentIndex != -1)
                {
                    commentColumn = commentIndex;
                    comments.Add(w.Substring(commentIndex).Trim());
                    w = w.Substring(0, commentIndex - 1);
                }
                else
                {
                    comments.Add(null);
                }
                return w.Trim();
            });

            var changeMarker = _configurationService.Configuration.ChangeMarker switch
            {
                Models.ChangeMarker.NoChangeMarker => ChangeMarker.NoChangeMarker,
                Models.ChangeMarker.UnderlineChangeMarker => ChangeMarker.Underline,
                Models.ChangeMarker.DoubleUnderlineChangeMarker => ChangeMarker.DoubleUnderline,
                Models.ChangeMarker.CaretChangeMarker => ChangeMarker.Caret,
                _ => ChangeMarker.DoubleUnderline
            };

            _errorService.ClearErrors();
            var sw = new Stopwatch();
            sw.Start();
            var (result, totalCompileTime) = await compiledRules.ApplyRules(lexicon, changeMarker);
            sw.Stop();
            IsLexiconTransformed = true;

            var warnings = result.SelectMany(r => r.errors.Select(e => $"Rule #{e.Item1}: {e.Item2}"));
            _errorService.AddWarnings(warnings);

            Status = $"Transformed {lexicon.Count()} words in {sw.ElapsedMilliseconds} ms";

            return (result, commentColumn, comments);
        }
    }
}
