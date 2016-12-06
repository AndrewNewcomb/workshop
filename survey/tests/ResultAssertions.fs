module ResultAssertions
open Micromachine

let assertOk x =
    match x with
    | Ok _ -> true
    | _ -> false

let isOk v x =
    match x with
    | Ok y when v = y -> true
    | _ -> false

let assertError x =
    match x with
    | Error x -> true
    | _ -> false

let isError v x =
    match x with
    | Error y when v = y -> true
    | _ -> false
