#r @"packages/build/FAKE/tools/FakeLib.dll"
open Fake
open Fake.Testing.XUnit

Target "Build" (fun _ ->
    !! "**/*.fsproj"
    |> MSBuildRelease null "Build"
    |> Log "Build Output:"
)

RunTargetOrDefault "Build"
