// Project:     Transmute.Desktop
// Module:      Combo box extension methods
// Copyright:   (c) 2026 Matt Arriola
// License:     MIT

using Avalonia.Controls;
using Avalonia.Controls.Primitives;
using Avalonia.LogicalTree;
using Avalonia.VisualTree;

namespace Transmute.Desktop.Extensions
{
    internal static class ComboBoxExtensions
    {
        extension(ComboBox cb)
        {
            public ScrollViewer ScrollViewer => cb
                .FindDescendantOfType<Popup>()?
                .FindLogicalDescendantOfType<Border>()?
                .FindLogicalDescendantOfType<ScrollViewer>()!;
        }
    }
}
