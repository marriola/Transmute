// Project:     Transmute.Desktop
// Module:      Utilities
// Copyright:   (c) 2026 Matt Arriola
// License:     MIT

using System.Diagnostics;
using System.Runtime.InteropServices;

namespace Transmute.Desktop
{
    internal static class Utilities
    {
        public static void OpenWebPage(string url)
        {
            if (RuntimeInformation.IsOSPlatform(OSPlatform.Windows))
            {
                // https://stackoverflow.com/a/2796367/241446
                using var proc = new Process { StartInfo = { UseShellExecute = true, FileName = url } };
                proc.Start();
            }
            else if (RuntimeInformation.IsOSPlatform(OSPlatform.Linux))
            {
                Process.Start("x-www-browser", url);
            }
            else if (RuntimeInformation.IsOSPlatform(OSPlatform.OSX))
            {
                Process.Start("open", url);
            }
        }
    }
}
