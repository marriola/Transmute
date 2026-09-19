// Project:     Transmute.Desktop
// Module:      Service collection extension methods
// Copyright:   (c) 2026 Matt Arriola
// License:     MIT

using Microsoft.Extensions.DependencyInjection;
using Transmute.Desktop.Services;

namespace Transmute.Desktop.Extensions
{
    internal static class ServiceCollectionExtensions
    {
        public static void AddCommonServices(this ServiceCollection services)
        {
            services.AddSingleton<ConfigurationService>();
            services.AddSingleton<ErrorService>();
            services.AddSingleton<HelpService>();
        }
    }
}
