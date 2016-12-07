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

type Response with
    static member ToJson(response) : JsonValue =
        match response with
        | FreeForm -> jobj [| "responseType" .= "FreeForm" |]
        | Choice s -> jobj [| "responseType" .= "Choice"; "responseText" .= s |]
    static member FromJson(_:Response) =
        parseObj <| fun json -> jsonParse {
            let! responseType = json .@ "responseType"
            let! responseText = json .@? "responseText"

            let response = 
                match responseType, responseText with
                | "FreeForm", _ -> FreeForm
                | "Choice", Some s -> Choice s
                | _ -> failwith "Could not parse Response FromJson"

            return response}

type Question with

    static member ToJson(question) : JsonValue =
            jobj [| "question" .= question.question; "responses" .= question.responses |]

    static member FromJson(_:Question) =
        parseObj <| fun json -> jsonParse {
            let! question = json .@ "question"
            let! responses = json .@ "responses"

            return {question=question; responses=responses}}

type SurveyAction with

    static member ToJson(action) : JsonValue =
        match action with
        | Authored(title, author) ->
            jobj [| "action" .= "authored"; "title" .= title; "author" .= author |]
        | QuestionAdded(id, question) ->
            jobj [| "action" .= "questionAdded"; "id" .= id; "question" .= question |]
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
                return QuestionAdded(id, question)
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