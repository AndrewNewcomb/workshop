#load "../refs.fsx"

open FSharp.Control
open FSharp.Data
open Micromachine

type Example =
  { sample : string
    answer : int option
  }
with

    static member ToJson(ex) : JsonValue =
        jobj [| "sample" .= ex.sample; "ans" .= ex.answer |]

    static member FromJson(_:Example) =
        parseObj <| fun json -> jsonParse {
            let! sample = json .@ "sample"
            let! answer = json .@? "ans"
            return {sample = sample; answer = answer} }

let ex : JsonResult<Example> = Example.ToJson({sample = "hello"; answer = Some 42}) |> fromJson

//-----------

type HasTitles = {titles: string[]}
with
    static member ToJson(ex) : JsonValue =
        jobj [| "titles" .= ex.titles |]
    static member FromJson(_:HasTitles) =
        parseObj <| fun json -> jsonParse {
            let! (titles:string[]) = json .@ "titles"
            return {titles = titles} }

type Example2 =
  { sample : string
    answer : int option
    titles: HasTitles  // was if it was just string[]
  }
with

    static member ToJson(ex) : JsonValue =
        jobj [| "sample" .= ex.sample; "ans" .= ex.answer; "titles" .= ex.titles |]

    static member FromJson(_:Example2) =
        parseObj <| fun json -> jsonParse {
            let! sample = json .@ "sample"
            let! answer = json .@? "ans"
            //let! (strings:string []) = json .@ "strings"
            let! (titles:HasTitles) = json .@ "titles"
            return {sample = sample; answer = answer; titles = titles} }

let ex2 : JsonResult<Example2> = Example2.ToJson({sample = "hello"; answer = Some 42; titles = { titles= [|"a";"b";"c"|]}}) |> fromJson
