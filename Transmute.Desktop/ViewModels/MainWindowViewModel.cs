// Project:     Transmute.Desktop
// Module:      Main window view model
// Copyright:   (c) 2026 Matt Arriola
// License:     MIT

using Avalonia.Controls.Primitives;
using System.Threading.Tasks;
using Transmute.Desktop.Models;
using Transmute.Desktop.Services;
using Transmute.Engine;

namespace Transmute.Desktop.ViewModels
{
    public partial class MainWindowViewModel : ViewModelBase
    {
        public InputFormat[] InputFormats { get; } = [InputFormat.IPA, InputFormat.X_SAMPA];

        public ScrollBarVisibility HorizontalScrollBarVisibility
        {
            get;

            set
            {
                field = value;
                InputPaneViewModel?.HorizontalScrollBarVisibility = value;
                OutputPaneViewModel?.HorizontalScrollBarVisibility = value;
            }
        }

        public MenuViewModel MenuViewModel { get; set; } = new();

        public RulesPaneViewModel RulesPaneViewModel { get; set; } = new();

        public InputPaneViewModel InputPaneViewModel { get; set; } = new();

        public OutputPaneViewModel OutputPaneViewModel { get; set; } = new();

        public Configuration Configuration => _configurationService.Configuration;

        private ConfigurationService _configurationService = GetService<ConfigurationService>()!;

        public void CloseErrors()
        {
            RulesPaneViewModel.ErrorPaneViewModel.ErrorsTab = null;
        }

        public async Task<bool> ApplyRules()
        {
            if (string.IsNullOrWhiteSpace(InputPaneViewModel.InputLexicon) || !await RulesPaneViewModel.CompileRules())
            {
                return false;
            }

            CloseErrors();
            var (result, commentColumn, comments) = await InputPaneViewModel.ApplyRules(RulesPaneViewModel.CompiledRules!);

            if (result == null)
            {
                return false;
            }

            OutputPaneViewModel.LoadOutputLexicon(result, commentColumn, comments);

            return true;
        }
    }
}
