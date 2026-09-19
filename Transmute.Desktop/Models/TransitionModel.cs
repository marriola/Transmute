// Project:     Transmute.Desktop
// Module:      Transition model
// Copyright:   (c) 2026 Matt Arriola
// License:     MIT

namespace Transmute.Desktop.Models
{
    public class TransitionModel
    {
        public required int Index { get; set; }

        public required string Origin { get; set; }
        
        public required string Input { get; set; }

        public required string Destination { get; set; }

        public bool IsFinal { get; set; }

        public string? Output { get; set; }
    }
}
