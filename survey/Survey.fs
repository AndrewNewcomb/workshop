module Survey
open FSharp.Control
open FSharp.Data
open Micromachine

exception SurveyDoesNotExist
exception SurveyAlreadyPublished
exception SurveyAlreadyClosed
exception SurveyIsEmpty

type SurveyAction =
    | Authored of title:string * author:string
    | QuestionAdded of id:string * Question
    | QuestionRemoved of id:string * Question
    | Published of System.DateTime
    | Closed of System.DateTime
and Question =
  { question : string
    responses : Response[] }
and Response =
    | FreeForm
    | Choice of string

type SurveyAction with

    static member ToJson(action) : JsonValue =
        match action with
        | Authored(title, author) ->
            jobj [| "action" .= "authored"; "title" .= title; "author" .= author |]
        | _ ->
            jobj [| "action" .= "todo" |]

    static member FromJson(_:SurveyAction) =
        parseObj <| fun json -> jsonParse {
            let! action = json .@ "action"
            match action with
            | "authored" ->
                let! title = json .@ "title"
                let! author = json .@ "author"
                return Authored(title, author)
            | unknown ->
                return failwithf "unimplemented action: %s" unknown }

let reduce (actions:AsyncSeq<SurveyAction>) (* : Result<??, exn> *) =
    failwith "implement me using SurveyActionValidation"
