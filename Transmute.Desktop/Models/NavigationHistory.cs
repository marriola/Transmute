using System;

namespace Transmute.Desktop.Models
{
    public record NavigationHistory(int Line, int Position, string Description = "")
    {
    }
}
