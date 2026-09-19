// Project:     Transmute.Desktop
// Module:      Help service
// Copyright:   (c) 2026 Matt Arriola
// License:     MIT

using Avalonia.Controls.ApplicationLifetimes;
using System;
using System.Collections.Generic;
using System.Formats.Tar;
using System.IO;
using System.IO.Compression;
using System.Text.Json;
using Transmute.Desktop.ViewModels;

namespace Transmute.Desktop.Services
{
    public class HelpService
    {
        public List<HelpSectionViewModel> Sections { get; private set; }

        public string WebViewFolder { get; private set; } = Path.Join(Path.GetTempPath(), "Transmute.Desktop.WebView");

        public string? HelpFolder { get; private set; } = null;

        private readonly JsonSerializerOptions _jsonSerializerOptions = new() { TypeInfoResolver = AppJsonContext.Default };

        public async void Initialize()
        {
            if (Sections != null)
            {
                return;
            }

            try
            {
                Directory.Delete(WebViewFolder, true);
            }
            catch { }

            HelpFolder = Path.Join(Path.GetTempPath(), "Transmute.Desktop.Help");
            Directory.CreateDirectory(HelpFolder);

            using var archiveStream = new MemoryStream(Help.Archive);
            using var gzipStream = new GZipStream(archiveStream, CompressionMode.Decompress);
            await TarFile.ExtractToDirectoryAsync(gzipStream, HelpFolder, overwriteFiles: true);

            using var tocReader = new StreamReader(Path.Combine(HelpFolder, "toc.json"));
            Sections = JsonSerializer.Deserialize<List<HelpSectionViewModel>>(await tocReader.ReadToEndAsync(), _jsonSerializerOptions)!;

            foreach (var section in Sections)
            {
                SetParents(section);
            }

            if (App.Current.ApplicationLifetime is IClassicDesktopStyleApplicationLifetime desktop)
            {
                desktop.Exit += DeleteTempFolder;
            }
        }

        private void SetParents(HelpSectionViewModel section, HelpSectionViewModel? parent = null)
        {
            section.Parent = parent;

            foreach (var child in section.Children)
            {
                SetParents(child, parent: section);
            }
        }

        private async void DeleteTempFolder(object? sender, ControlledApplicationLifetimeExitEventArgs e)
        {
            if (HelpFolder != null)
            {
                try
                {
                    Directory.Delete(HelpFolder, true);
                }
                catch (Exception ex)
                {
                    Console.WriteLine(ex.ToString());
                }
            }
        }
    }
}
