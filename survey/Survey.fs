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
    responses : Response [] }
and Response =
    | FreeForm
    | Choice of string

type SurveyAction with

    static member ToJson(action) : JsonValue =
        match action with
        | Authored(title, author) ->
            jobj [| "action" .= "authored"; "title" .= title; "author" .= author |]
        | QuestionAdded(id, question) ->

// wanted to serialise the responses array, but have not figured it out
//            let responseSer response =
//                match response with
//                | FreeForm -> jobj [| "FreeFrom" .= "FreeForm" |]
//                | Choice s -> jobj [| "Choice" .= s |]
//
//            let questionResponses = question.responses |> Array.map responseSer 

            let serResponse =
                if question.responses = [|FreeForm|]
                then "FreeForm"
                else failwith "Choice is not supported"
                
            jobj [| "action" .= "questionAdded"; "id" .= id; "question" .= question.question; "responses" .= serResponse |]
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
            | "questionAdded" ->
                let! id = json .@ "id"
                let! question = json .@ "question"

// wanted to serialise the responses array, but have not figured it out
//                let! responsesCombined = json .@ "responses"

                let! resp = json .@ "responses"
                let responses =
                    if resp = "FreeForm"
                    then [| FreeForm |]
                    else failwith "Choice is not supported"

                let q = {question=question; responses=responses}
                return QuestionAdded(id, q)
            | unknown ->
                return failwithf "unimplemented action: %s" unknown }

type State =
    | SurveyCreated
    | SurveyPublished
    | SurveyClosed
    | SurveyCancelled

type SurveyState = 
    {        
        title: string
        author: string
        questions: Map<string, Question>
        state: State
    }
    
let tryPerformAction survey action =
    let initialise = 
        match action with
        | Authored(title, author) ->
            Ok {
                title = title
                author = author
                questions = Map.empty
                state = SurveyCreated}
        | _ ->
            Error <| SurveyDoesNotExist 
    


    let nextChoiceAfterAction incomingSurveyState =
        match incomingSurveyState.state, action with
        | SurveyCreated, Authored(title, author) ->
            Ok { incomingSurveyState with title = title; author = author}
        | SurveyCreated, QuestionAdded(key, question) ->  
            Ok { incomingSurveyState with questions = incomingSurveyState.questions.Add(key, question)}
        | SurveyCreated, Published(publishedDate) ->     
            match incomingSurveyState.questions.IsEmpty with
            | false -> Ok { incomingSurveyState with state = SurveyPublished}
            | true -> Error <| SurveyIsEmpty
        | SurveyPublished, Closed(closedDate) ->     
            Ok { incomingSurveyState with state = SurveyClosed}
        | SurveyPublished, Published(publishedDate) ->  
            Error <| SurveyAlreadyPublished
        | SurveyClosed, Closed(closedDate) ->  
            Error <| SurveyAlreadyPublished
        | _ ->
            failwith "not implemented yet"            
                  
    let nextState =
        match survey with
        | None -> initialise                     
        | Some incomingChoice -> 
            match incomingChoice with
            | Ok incomingState -> nextChoiceAfterAction incomingState 
            | Error exn -> incomingChoice

    let result = Some nextState
    result

    
let reduce (actions:AsyncSeq<SurveyAction>) : Async<Choice<SurveyState, exn>> = async {
    let! result = actions |> AsyncSeq.fold tryPerformAction None
    match result with
    | Some result -> return result
    | None -> return Error <| SurveyDoesNotExist
}