// Project:     Transmute.Desktop
// Module:      Go To Line dialog
// Copyright:   (c) 2026 Matt Arriola
// License:     MIT

using Avalonia.Controls;

namespace Transmute.Desktop;

public partial class GoToLineWindow : Window
{
    public GoToLineWindow()
    {
        InitializeComponent();
    }

    private void GoButton_Click(object? sender, Avalonia.Interactivity.RoutedEventArgs e)
    {
        Close((int?)LineNumber?.Value);
    }

    private void CancelButton_Click(object? sender, Avalonia.Interactivity.RoutedEventArgs e)
    {
        Close();
    }

    private void Window_Opened(object? sender, System.EventArgs e)
    {
        LineNumber.Focus();
    }

    public void Go() => GoButton_Click(null, null);

    public void Cancel() => CancelButton_Click(null, null);

    private void LineNumber_ValueChanged(object? sender, NumericUpDownValueChangedEventArgs e)
    {
        GoButton.IsEnabled = !string.IsNullOrEmpty(LineNumber.Text);
    }
}