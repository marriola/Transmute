// Project:     Transmute.Desktop
// Copyright:   (c) 2026 Matt Arriola
// License:     MIT

using Avalonia;
using Optris.Icons.Avalonia;
using Optris.Icons.Avalonia.FontAwesome;
using System;

namespace Transmute.Desktop
{
    internal sealed class Program
    {
        public static string[] Arguments { get; private set; }

        // Initialization code. Don't use any Avalonia, third-party APIs or any
        // SynchronizationContext-reliant code before AppMain is called: things aren't initialized
        // yet and stuff might break.
        [STAThread]
        public static void Main(string[] args)
        {
            Arguments = args;
            BuildAvaloniaApp().StartWithClassicDesktopLifetime(args);
        }

        // Avalonia configuration, don't remove; also used by visual designer.
        public static AppBuilder BuildAvaloniaApp()
        {
            IconProvider.Current.Register<FontAwesomeIconProvider>();
            
            return AppBuilder.Configure<App>()
                        .UsePlatformDetect()
                        .WithInterFont()
                        .LogToTrace();
        }
    }
}
