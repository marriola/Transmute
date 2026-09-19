// Project:     Transmute.Desktop
// Module:      Font dialog
// Copyright:   (c) 2026 Matt Arriola
// License:     MIT

using Avalonia.Controls;
using Avalonia.Media;
using System.Collections.Generic;
using System.Linq;

namespace Transmute.Desktop;

public partial class FontDialog : Window
{
    public int[] FontSizes { get; set; } = Enumerable.Range(6, 19).ToArray();

    public FontFamily SelectedRulesFont { get; set; }

    public int SelectedRulesFontSize { get; set; }
    
    public FontFamily SelectedLexiconFont { get; set; }

    public int SelectedLexiconFontSize { get; set; }

    public FontFamily SelectedGridFont { get; set; }

    public int SelectedGridFontSize { get; set; }

    public List<FontFamily> Fonts { get; set; }

    public FontDialog()
    {
        InitializeComponent();
        Fonts = FontManager.Current.SystemFonts.OrderBy(x => x.Name).ToList();
        DataContext = this;
    }

    private void OK_Click(object? sender, Avalonia.Interactivity.RoutedEventArgs e)
    {
        Close((SelectedRulesFont, SelectedRulesFontSize, SelectedLexiconFont, SelectedLexiconFontSize, SelectedGridFont, SelectedGridFontSize));
    }

    private void Cancel_Click(object? sender, Avalonia.Interactivity.RoutedEventArgs e)
    {
        Close();
    }
}