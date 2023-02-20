namespace TransmuteLib.Utils

module Lzma =
#if FABLE_COMPILER
    ()
#else
    open Joveler.Compression.XZ
    open System.IO
    open System.Runtime.InteropServices

    let mutable private didInit = false

    let init () =
        if not didInit then
            let libDir = "runtimes"

            let libDir = 
                if RuntimeInformation.IsOSPlatform(OSPlatform.Windows)
                    then Path.Combine(libDir, "win-")
                elif RuntimeInformation.IsOSPlatform(OSPlatform.Linux)
                    then Path.Combine(libDir, "linux-")
                elif RuntimeInformation.IsOSPlatform(OSPlatform.OSX)
                    then Path.Combine(libDir, "osx-")
                else
                    ""

            let libDir =
                match RuntimeInformation.ProcessArchitecture with
                | Architecture.X86 -> libDir + "x86"
                | Architecture.X64 -> libDir + "x64"
                | Architecture.Arm -> libDir + "arm"
                | Architecture.Arm64 -> libDir + "arm64"

            let libDir = Path.Combine(libDir, "native")

            let libPath =
                if RuntimeInformation.IsOSPlatform(OSPlatform.Windows)
                    then Path.Combine(libDir, "liblzma.dll")
                elif RuntimeInformation.IsOSPlatform(OSPlatform.Linux)
                    then Path.Combine(libDir, "liblzma.so")
                elif RuntimeInformation.IsOSPlatform(OSPlatform.OSX)
                    then Path.Combine(libDir, "liblzma.dylib")
                else
                    ""

            XZInit.GlobalInit(libPath)
            didInit <- true
#endif
