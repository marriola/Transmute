// Project:     Transmute.Desktop
// Module:      Configuration service
// Copyright:   (c) 2026 Matt Arriola
// License:     MIT

using Avalonia.Styling;
using System;
using System.IO;
using System.Runtime.InteropServices;
using System.Text.Json;
using Transmute.Desktop.Models;

namespace Transmute.Desktop.Services
{
    public class ConfigurationService
    {
        private const string _configurationFilename = "config.json";

        public string ConfigurationDirectory { get; private set; } = "./";

        /// <summary>
        /// The file picker directory location used the first time the app is opened.
        /// </summary>
        public string InitialDirectory { get; private set; } = "./";

        public delegate void ThemeChangeEventHandler(object sender, ThemeVariant themeVariant);

        public event ThemeChangeEventHandler ThemeChange;

        public Configuration Configuration { get; private set; } = new Configuration();

        public bool IsNew { get; private set; } = true;

        private string ConfigurationPath => Path.Join(ConfigurationDirectory, _configurationFilename);

        public ConfigurationService()
        {
            if (RuntimeInformation.IsOSPlatform(OSPlatform.Windows))
            {
                ConfigurationDirectory = Path.Join(Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData), "transmute.desktop");
                InitialDirectory = Path.Join(Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments), "Transmute");
            }
            else if (RuntimeInformation.IsOSPlatform(OSPlatform.OSX))
            {
                ConfigurationDirectory = Path.Join(Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData), "com.mattarriola.transmute.desktop");
                InitialDirectory = Path.Join(Environment.GetFolderPath(Environment.SpecialFolder.CommonApplicationData), "Transmute");
            }
            else if (!string.IsNullOrEmpty(Environment.ProcessPath))
            {
                ConfigurationDirectory = Path.Join(Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData), "transmute.desktop");
                InitialDirectory = Path.Join(Environment.GetFolderPath(Environment.SpecialFolder.CommonApplicationData), "transmute");
            }

            Load();
        }

        public void ChangeTheme(ThemeVariant themeVariant)
        {
            ThemeChange.Invoke(this, themeVariant);
            Configuration.Theme = themeVariant.Key.ToString();
        }

        public void Load()
        {
            Directory.CreateDirectory(ConfigurationDirectory);

            if (File.Exists(ConfigurationPath))
            {
                using var reader = File.OpenRead(ConfigurationPath);
                Configuration = JsonSerializer.Deserialize(reader, SourceGenerationContext.Default.Configuration) ?? Configuration;
                IsNew = false;
            }

            if (string.IsNullOrWhiteSpace(Configuration.CurrentDirectory))
            {
                Configuration.CurrentDirectory = InitialDirectory;
            }
        }

        public void Save()
        {
            using var writer = new StreamWriter(ConfigurationPath);
            writer.Write(JsonSerializer.Serialize(Configuration, SourceGenerationContext.Default.Configuration));
        }
    }
}
