// Project:     Transmute.Desktop
// Module:      Rules pane navigation list entry
// Copyright:   (c) 2026 Matt Arriola
// License:     MIT

using System.Linq;

namespace Transmute.Desktop.Models
{
    public record NavigationEntry(int? lineNumber, string label, string? description = null, bool showLineNumber = true, bool isSeparator = false)
    {
        public string Description => string.Join(": ", new object?[] { lineNumber, label, description }.Where(x => !string.IsNullOrWhiteSpace(x?.ToString())));
    }
}
