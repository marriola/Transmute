// Project:     Transmute.Desktop
// Module:      Rules pane navigation list entry
// Copyright:   (c) 2026 Matt Arriola
// License:     MIT

namespace Transmute.Desktop.Models
{
    public record NavigationEntry(int lineNumber, string description, bool showLineNumber = true)
    {
        public string Description => showLineNumber
            ? $"{lineNumber}: {description}"
            : description;
    }
}
