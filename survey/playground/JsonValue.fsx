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
