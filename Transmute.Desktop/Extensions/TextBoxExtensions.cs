// Project:     Transmute.Desktop
// Module:      Text box extension methods
// Copyright:   (c) 2026 Matt Arriola
// License:     MIT

using Avalonia.Controls;
using Avalonia.Threading;
using System.Threading.Tasks;

namespace Transmute.Desktop.Extensions
{
    internal static class TextBoxExtensions
    {
        extension(AvaloniaEdit.TextEditor textEditor)
        {
            public int LineNumber
            {
                get
                {
                    var currentLine = 1;

                    for (var i = 0; i < textEditor.CaretOffset; i++)
                    {
                        if (textEditor.Text[i] == '\n')
                        {
                            currentLine++;
                        }
                    }

                    return currentLine;
                }
            }
        }

        extension(TextBox textEditor)
        {
            public int LineNumber
            {
                get
                {
                    var currentLine = 1;

                    for (var i = 0; i < textEditor.CaretIndex; i++)
                    {
                        if (textEditor.Text[i] == '\n')
                        {
                            currentLine++;
                        }
                    }

                    return currentLine;
                }
            }
        }
        internal static async Task HighlightLine(this TextBox textBox, int lineNumber, bool goToHighlight = true)
        {
            if (!goToHighlight)
            {
                return;
            }

            var text = textBox.Text;
            var currentLine = 1;
            var startOffset = 0;
            var endOffset = 0;

            for (var i = 0; i < text.Length && currentLine < lineNumber; i++, startOffset++)
            {
                if (text[i] == '\n')
                {
                    currentLine++;
                }
            }

            for (var i = startOffset + 1; i < text.Length; i++)
            {
                if (text[i] == '\n')
                {
                    endOffset = i;
                    break;
                }
            }

            await HighlightRange(textBox, startOffset, endOffset);
        }

        internal static async Task HighlightRange(this TextBox textBox, int startOffset, int endOffset, bool goToHighlight = true, int duration = 500)
        {
            if (!goToHighlight)
            {
                return;
            }

            Dispatcher.UIThread.Post(async () =>
            {
                textBox.CaretIndex = startOffset;
                textBox.SelectionStart = startOffset;
                textBox.SelectionEnd = endOffset;
                await Task.Delay(duration);
                textBox.ClearSelection();
                textBox.CaretIndex = startOffset;
            });
        }

        internal static async Task HighlightLine(this AvaloniaEdit.TextEditor textBox, int lineNumber, bool goToHighlight = true)
        {
            var (startOffset, endOffset) = textBox.GetLineRange(lineNumber);

            Dispatcher.UIThread.Post(async () =>
            {
                if (!goToHighlight)
                {
                    textBox.CaretOffset = startOffset;
                    return;
                }

                textBox.CaretOffset = startOffset;
                textBox.SelectionStart = startOffset;
                textBox.SelectionLength = endOffset - startOffset + 1;
                await Task.Delay(500);
                textBox.SelectionLength = 0;
                textBox.CaretOffset = startOffset;
            });
        }

        private static (int startOffset, int endOffset) GetLineRange(this AvaloniaEdit.TextEditor textBox, int lineNumber)
        {
            var text = textBox.Text;
            var currentLine = 1;
            var startOffset = 0;
            var endOffset = 0;

            for (var i = 0; i < text.Length && currentLine < lineNumber; i++, startOffset++)
            {
                if (text[i] == '\n')
                {
                    currentLine++;
                }
            }

            for (var i = startOffset + 1; i < text.Length; i++)
            {
                if (text[i] == '\n')
                {
                    endOffset = i;
                    break;
                }
            }

            return (startOffset, endOffset);
        }
    }
}
