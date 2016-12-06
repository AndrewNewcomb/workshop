module SurveyActionValidation
open Swensen.Unquote
open Xunit
open Micromachine
open ResultAssertions
open FSharp.Control

let sometime () = System.DateTime.Now

let freeformQ q : Survey.Question =
  { question = q
    responses = [|Survey.FreeForm|] }

let reduceSurvey xs =
    xs |> AsyncSeq.ofSeq |> Survey.reduce |> Async.RunSynchronously

[<Fact>]
let ``The first event must be Authored`` () =
    let result =
        [Survey.Authored("Test Survey", "F# Hacker")]
        |> reduceSurvey
    test <@ result |> assertOk @>
    [ Survey.Published(sometime ())
      Survey.Closed(sometime ())
      Survey.QuestionAdded("color", freeformQ "What is your favorite color?")
      Survey.QuestionRemoved("quest", freeformQ "What is your quest?")
    ] |> Seq.iter (fun action ->
        let result = [action] |> reduceSurvey
        test <@ result |> isError Survey.SurveyDoesNotExist @>)

[<Fact>]
let ``We need at least one question in order to publish a survey`` () =
    let result = 
        [ Survey.Authored("Test Survey", "F# Hacker")
          Survey.Published(sometime ())
        ] |> reduceSurvey
    test <@ result |> isError Survey.SurveyIsEmpty @>

[<Fact>]
let ``We can't publish a survey more than once`` () =
    let result =
        [ Survey.Authored("Test Survey", "F# Hacker")
          Survey.QuestionAdded("quest", freeformQ "What is your quest?")
          Survey.Published(sometime ())
          Survey.Published(sometime ())
        ] |> reduceSurvey
    test <@ result |> isError Survey.SurveyAlreadyPublished @>

[<Fact>]
let ``We can't close a survey more than once`` () =
    let result =
        [ Survey.Authored("Test Survey", "F# Hacker")
          Survey.QuestionAdded("quest", freeformQ "What is your quest?")
          Survey.Published(sometime ())
          Survey.Closed(sometime ())
          Survey.Closed(sometime ())
        ] |> reduceSurvey
    test <@ result |> isError Survey.SurveyAlreadyClosed @>

[<Fact>]
let ``The last authorship event should supersede others`` () =
    let resultA =
        [ Survey.Authored("Test Survey", "F# Hacker") ] |> reduceSurvey
    let resultB =
        [ Survey.Authored("Someother Name", "Someone Else")
          Survey.Authored("Test Survey", "F# Hacker") ] |> reduceSurvey
    test <@ resultA = resultB @>
    test <@ resultA |> assertOk @>
