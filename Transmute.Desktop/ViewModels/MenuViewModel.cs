// Project:     Transmute.Desktop
// Module:      Menu view model
// Copyright:   (c) 2026 Matt Arriola
// License:     MIT

using Avalonia.Styling;
using Transmute.Desktop.Models;
using Transmute.Desktop.Services;

namespace Transmute.Desktop.ViewModels
{
    public class MenuViewModel : ViewModelBase
    {
        public ChangeMarker NoChangeMarker => ChangeMarker.NoChangeMarker;
        public ChangeMarker UnderlineChangeMarker => ChangeMarker.UnderlineChangeMarker;
        public ChangeMarker DoubleUnderlineChangeMarker => ChangeMarker.DoubleUnderlineChangeMarker;
        public ChangeMarker CaretChangeMarker => ChangeMarker.CaretChangeMarker;

        public bool NoChangeMarkerSelected => _configurationService.Configuration.ChangeMarker == ChangeMarker.NoChangeMarker;
        public bool UnderlineChangeMarkerSelected => _configurationService.Configuration.ChangeMarker == ChangeMarker.UnderlineChangeMarker;
        public bool DoubleUnderlineChangeMarkerSelected => _configurationService.Configuration.ChangeMarker == ChangeMarker.DoubleUnderlineChangeMarker;
        public bool CaretChangeMarkerSelected => _configurationService.Configuration.ChangeMarker == ChangeMarker.CaretChangeMarker;

        public bool SystemDefaultThemeSelected => _configurationService.Configuration.ThemeVariant == ThemeVariant.Default;
        public bool LightThemeSelected => _configurationService.Configuration.ThemeVariant == ThemeVariant.Light;
        public bool DarkThemeSelected => _configurationService.Configuration.ThemeVariant == ThemeVariant.Dark;

        private ConfigurationService _configurationService = GetService<ConfigurationService>()!;
    }
}
