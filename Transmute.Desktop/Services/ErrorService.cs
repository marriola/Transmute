// Project:     Transmute.Desktop
// Module:      Error and warning service
// Copyright:   (c) 2026 Matt Arriola
// License:     MIT

using System;
using System.Collections.Generic;

namespace Transmute.Desktop.Services
{
    public class ErrorService
    {
        private List<string> errors = [];

        private List<string> warnings = [];

        public IReadOnlyList<string> Errors => errors.AsReadOnly();

        public IReadOnlyList<string> Warnings => warnings.AsReadOnly();

        public event Action? Cleared;
        public event Action<IEnumerable<string>>? ErrorsSet;
        public event Action<IEnumerable<string>>? WarningsSet;

        public void ClearErrors()
        {
            errors.Clear();
            warnings.Clear();
            Cleared?.Invoke();
        }

        public void AddErrors(IEnumerable<string> error)
        {
            errors.AddRange(error);
            ErrorsSet?.Invoke(errors);
        }

        public void AddWarnings(IEnumerable<string> warning)
        {
            warnings.AddRange(warning);
            WarningsSet?.Invoke(warnings);
        }
    }
}
