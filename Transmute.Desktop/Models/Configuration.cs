// Project:     Transmute.Desktop
// Module:      Configuration
// Copyright:   (c) 2026 Matt Arriola
// License:     MIT

using Avalonia.Styling;
using System.Text.Json.Serialization;

namespace Transmute.Desktop.Models
{
    public class Configuration
    {
        public string? Theme { get; set; }

        [JsonIgnore]
        public ThemeVariant ThemeVariant => Theme switch
        {
            "Light" => ThemeVariant.Light,
            "Dark" => ThemeVariant.Dark,
            _ => ThemeVariant.Default
        };

        public bool ShowHorizontalScrollbar { get; set; }

        public bool WordWrap { get; set; } = true;

        /// <summary>
        /// Rules font
        /// </summary>
        public string RulesFontFamily { get; set; }

        /// <summary>
        /// Rules font size
        /// </summary>
        public int RulesFontSize { get; set; }

        /// <summary>
        /// Lexicon font
        /// </summary>
        public string LexiconFontFamily { get; set; }

        /// <summary>
        /// Lexicon font size
        /// </summary>
        public int LexiconFontSize { get; set; }

        /// <summary>
        /// Grid font
        /// </summary>
        public string GridFontFamily { get; set; }

        /// <summary>
        /// Grid font size
        /// </summary>
        public int GridFontSize { get; set; }

        /// <summary>
        /// Change marker
        /// </summary>
        public ChangeMarker? ChangeMarker { get; set; }

        /// <summary>
        /// The current file picker directory
        /// </summary>
        public string? CurrentDirectory { get; set; }

        public int WindowWidth { get; set; } = 800;

        public int WindowHeight { get; set; } = 600;

        public int WindowX { get; set; } = 0;

        public int WindowY { get; set; } = 0;

        public bool IsMaximized { get; set; } = true;
    }

    public enum ChangeMarker
    {
        NoChangeMarker = 0,
        UnderlineChangeMarker = 1,
        DoubleUnderlineChangeMarker = 2,
        CaretChangeMarker = 3
    }

    [JsonSourceGenerationOptions(WriteIndented = true)]
    [JsonSerializable(typeof(Configuration))]
    internal partial class SourceGenerationContext : JsonSerializerContext { }
}
