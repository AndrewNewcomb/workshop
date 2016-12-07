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
    // a first event of authored is ok
    let result =
        [Survey.Authored("Test Survey", "F# Hacker")]
        |> reduceSurvey
    test <@ result |> assertOk @>

    // check each of the other events as the first event to see they each fail
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
    test <@ result |> isError Survey.SurveyAlreadyPublished @>

[<Fact>]
let ``The last authorship event should supersede others`` () =
    let resultA =
        [ Survey.Authored("Test Survey", "F# Hacker") ] |> reduceSurvey
    let resultB =
        [ Survey.Authored("Someother Name", "Someone Else")
          Survey.Authored("Test Survey", "F# Hacker") ] |> reduceSurvey
    test <@ resultA = resultB @>
    test <@ resultA |> assertOk @>

[<Fact>]
let ``A question can be removed if not already published`` () =
    let result =
        [ Survey.Authored("Test Survey", "F# Hacker")
          Survey.QuestionAdded("qA", freeformQ "A")
          Survey.QuestionAdded("qB", freeformQ "B")
          Survey.QuestionRemoved("qA", freeformQ "Does not matter as uses the id")
          Survey.Published(sometime ())
        ] |> reduceSurvey

    test <@ result |> assertOk @>

    match result with 
    | Ok survey -> 
        test <@ survey.questions.ContainsKey("qB") @>
        test <@ survey.questions.Count = 1 @>
    | _ -> ()

[<Fact>]
let ``A question cannot be removed if already published`` () =
    let result =
        [ Survey.Authored("Test Survey", "F# Hacker")
          Survey.QuestionAdded("qA", freeformQ "A")
          Survey.QuestionAdded("qB", freeformQ "B")
          Survey.Published(sometime ())
          Survey.QuestionRemoved("qA", freeformQ "Does not matter as uses the id")
        ] |> reduceSurvey

    test <@ result |> isError Survey.SurveyAlreadyPublished @>