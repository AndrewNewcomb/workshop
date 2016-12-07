module SurveyJsonRoundTrip
open Swensen.Unquote
open Xunit
open FSharp.Data
open Micromachine

let sometimeNoMilliSeconds () = System.DateTime(2000,12,31,23,59,58,0, System.DateTimeKind.Local)

let roundTrip (action:Survey.SurveyAction) : Survey.SurveyAction =
    match (toJson >> fromJson) action with
    | Ok roundtrip -> roundtrip
    | Error err -> failwithf "parsing error: %s" err

[<Fact>]
let ``Authored should round trip`` () =
    let action = Survey.Authored("test", "author")
    test <@ action = roundTrip action @>

// TODO: Add tests for the rest (part 2 of service construction)

[<Fact>]
let ``QuestionAdded should round trip`` () =
    let action = Survey.QuestionAdded("qid", {question = "thequestion"; responses= [|Survey.FreeForm|]})
    let roundTripped = roundTrip action
    test <@ action = roundTripped @>

[<Fact>]
let ``Published should round trip`` () =
    let dt = sometimeNoMilliSeconds()
    let action = Survey.Published dt
    test <@ action = roundTrip action @>

[<Fact>]
let ``Closed should round trip`` () =
    let dt = sometimeNoMilliSeconds()
    let action = Survey.Closed dt
    test <@ action = roundTrip action @>