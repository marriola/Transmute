// Project:     Transmute.Desktop
// Module:      Transmute Avalonia app
// Copyright:   (c) 2026 Matt Arriola
// License:     MIT

using Avalonia;
using Avalonia.Controls;
using Avalonia.Controls.ApplicationLifetimes;
using Avalonia.Markup.Xaml;
using Microsoft.Extensions.DependencyInjection;
using System;
using Transmute.Desktop.Extensions;
using Transmute.Desktop.Services;
using Transmute.Desktop.ViewModels;
using Transmute.Desktop.Views;

namespace Transmute.Desktop
{
    public partial class App : Application
    {
        public static new App Current => Application.Current as App;

        public IServiceProvider? Services { get; private set; } = null;

        public bool IsShuttingDown { get; private set; }

        private ConfigurationService _configurationService;

        public override void Initialize()
        {
            AvaloniaXamlLoader.Load(this);
        }

        public override void OnFrameworkInitializationCompleted()
        {
            if (ApplicationLifetime is not IClassicDesktopStyleApplicationLifetime desktop)
            {
                return;
            }

            if (!Design.IsDesignMode)
            {
                var services = new ServiceCollection();
                services.AddCommonServices();
                Services = services.BuildServiceProvider();

                Services.GetService<HelpService>()!.Initialize();

                _configurationService = Services.GetService<ConfigurationService>()!;

                _configurationService.ThemeChange += (sender, themeVariant) =>
                {
                    RequestedThemeVariant = themeVariant;
                };
            }

            // Avoid duplicate validations from both Avalonia and the CommunityToolkit. 
            // More info: https://docs.avaloniaui.net/docs/guides/development-guides/data-validation#manage-validationplugins
            //DisableAvaloniaDataAnnotationValidation();
            desktop.MainWindow = new MainWindow
            {
                DataContext = new MainWindowViewModel(),
                Width = _configurationService.Configuration.WindowWidth,
                Height = _configurationService.Configuration.WindowHeight,
                WindowState = _configurationService.Configuration.IsMaximized ? WindowState.Maximized : WindowState.Normal,
                Position = new PixelPoint(_configurationService.Configuration.WindowX, _configurationService.Configuration.WindowY)
            };

            base.OnFrameworkInitializationCompleted();
        }

        public void Shutdown()
        {
            if (ApplicationLifetime is IClassicDesktopStyleApplicationLifetime desktop)
            {
                IsShuttingDown = true;
                desktop.Shutdown();
            }
        }

        //private void DisableAvaloniaDataAnnotationValidation()
        //{
        //    // Get an array of plugins to remove
        //    var dataValidationPluginsToRemove =
        //        BindingPlugins.DataValidators.OfType<DataAnnotationsValidationPlugin>().ToArray();

        //    // remove each entry found
        //    foreach (var plugin in dataValidationPluginsToRemove)
        //    {
        //        BindingPlugins.DataValidators.Remove(plugin);
        //    }
        //}
    }
}