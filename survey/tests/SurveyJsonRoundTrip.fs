module SurveyJsonRoundTrip
open Swensen.Unquote
open Xunit
open FSharp.Data
open Micromachine

let roundTrip (action:Survey.SurveyAction) : Survey.SurveyAction =
    match (toJson >> fromJson) action with
    | Ok roundtrip -> roundtrip
    | Error err -> failwithf "parsing error: %s" err

[<Fact>]
let ``Authored should round trip`` () =
    let action = Survey.Authored("test", "author")
    test <@ action = roundTrip action @>

// TODO: Add tests for the rest (part 2 of service construction)