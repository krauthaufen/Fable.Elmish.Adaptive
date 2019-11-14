#r "paket: groupref Build //"
#load ".fake/build.fsx/intellisense.fsx"

open System
open System.IO
open Fake.Core
open Fake.Core.TargetOperators
open Fake.DotNet
open Fake.IO
open Fake.IO.Globbing.Operators
open Fake.Tools
open System.Text.RegularExpressions

do Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let notes = ReleaseNotes.load "RELEASE_NOTES.md"

let isWindows =
    Environment.OSVersion.Platform <> PlatformID.Unix && Environment.OSVersion.Platform <> PlatformID.MacOSX

Target.create "Clean" (fun _ ->
    if Directory.Exists "bin/Debug" then
        Trace.trace "deleting bin/Debug"
        Directory.delete "bin/Debug"
    if Directory.Exists "bin/Release" then
        Trace.trace "deleting bin/Release"
        Directory.delete "bin/Release"

    let pkgs = !!"bin/*.nupkg" |> Seq.toList
    if not (List.isEmpty pkgs) then
        Trace.tracefn "deleting packages: %s" (pkgs |> Seq.map Path.GetFileNameWithoutExtension |> String.concat ", ")
        File.deleteAll pkgs


    let dirs = Directory.EnumerateDirectories("src", "obj", SearchOption.AllDirectories) |> Seq.toList

    if not (List.isEmpty dirs) then 
        for d in dirs do    
            let parent = Path.GetDirectoryName d
            let proj = Directory.GetFiles(parent, "*.fsproj") |> Seq.isEmpty |> not
            if proj then
                Trace.tracefn "deleting %s" d
                Directory.delete d
)

Target.create "DotNetCompile" (fun _ ->
    let options (o : DotNet.BuildOptions) =
        let v = sprintf "%d.%d.%d.%s" notes.SemVer.Major notes.SemVer.Minor notes.SemVer.Patch (string notes.SemVer.Build)

        { o with 
            Configuration = DotNet.BuildConfiguration.Release
            
            MSBuildParams = 
                { o.MSBuildParams with 
                    Properties = 
                        [
                            "GenerateAssemblyInfo", "true"
                            "AssemblyVersion", v
                            "FileVersion", v
                            "AssemblyFileVersion", v
                            "ProductVersion", v
                            "InformationalVersion", v
                        ] 
                }
        }
    DotNet.build options "Fable.Elmish.Adaptive.sln"
)

Target.create "NpmInstall" (fun _ ->
    let modules = "node_modules" |> Path.GetFullPath

    if not (Directory.Exists modules) then
        Trace.trace "running `npm install`"

        let npm =
            if isWindows then CreateProcess.fromRawCommand "cmd" ["/C"; "npm"; "install"; "--dev"]
            else CreateProcess.fromRawCommand "npm" ["install"]

        use s = new MemoryStream()
        npm
        |> CreateProcess.withWorkingDirectory Environment.CurrentDirectory
        |> CreateProcess.withStandardError (StreamSpecification.UseStream(true, s))
        |> CreateProcess.withStandardOutput  (StreamSpecification.UseStream(true, s))
        |> Proc.run
        |> ignore

)

Target.create "Watch" (fun _ ->
    let wpds = "node_modules/webpack-dev-server/bin/webpack-dev-server.js" |> Path.GetFullPath
    CreateProcess.fromRawCommand "node" [wpds]
    |> CreateProcess.withWorkingDirectory Environment.CurrentDirectory
    |> CreateProcess.withStandardError StreamSpecification.Inherit
    |> CreateProcess.withStandardOutput StreamSpecification.Inherit
    |> CreateProcess.ensureExitCode
    |> Proc.run
    |> ignore

)
Target.create "Fable" (fun _ ->
    let wpds = "node_modules/webpack-cli/bin/cli.js" |> Path.GetFullPath
    CreateProcess.fromRawCommand "node" [wpds; "-p"]
    |> CreateProcess.withWorkingDirectory Environment.CurrentDirectory
    |> CreateProcess.withStandardError StreamSpecification.Inherit
    |> CreateProcess.withStandardOutput StreamSpecification.Inherit
    |> CreateProcess.ensureExitCode
    |> Proc.run
    |> ignore

)

Target.create "Default" ignore

Target.create "Docs" (fun _ ->
    let path = Path.Combine(__SOURCE_DIRECTORY__, "packages/docs/FSharp.Compiler.Tools/tools/fsi.exe")
    let workingDir = "docs/tools"
    let args = "generate.fsx"
    let command, args = 
        if false (* EnvironmentHelper.isMono *) then "mono", sprintf "'%s' %s" path args 
        else path, args

    if Shell.Exec(command, args, workingDir) <> 0 then
        failwith "failed to generate docs"
)

Target.create "GenerateDocs" (fun _ ->
    let path = Path.Combine(__SOURCE_DIRECTORY__, "packages/docs/FSharp.Compiler.Tools/tools/fsi.exe")
    let workingDir = "docs/tools"
    let args = "--define:RELEASE generate.fsx"
    let command, args = 
        if false (* EnvironmentHelper.isMono *) then "mono", sprintf "'%s' %s" path args 
        else path, args

    if Shell.Exec(command, args, workingDir) <> 0 then
        failwith "failed to generate docs"
)

"NpmInstall" ==> 
    "DotNetCompile" ==>
    "Watch"

"NpmInstall" ==> 
    "DotNetCompile" ==>
    "Fable"

"DotNetCompile" ==> 
    "Docs"
    
"DotNetCompile" ==> 
    "GenerateDocs"

"Fable" ==> 
    "Default"


Target.runOrDefault "Default"


