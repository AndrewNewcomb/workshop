namespace Micromachine

[<AutoOpen>]
module Pervasive =

    type Result<'a, 'b> = Choice<'a, 'b>

    /// Active pattern for matching a Choice<'a, 'b> with Choice1Of2 = Ok and Choice2Of2 = Error
    let inline (|Ok|Error|) c = c

    /// Indicates ok as Choice1Of2
    let inline Ok v = Choice1Of2 v

    /// Indicates error as Choice2Of2
    let inline Error e = Choice2Of2 e

    /// Null-coalescing operator.
    let (|?) lhs rhs = if lhs = null then rhs else lhs

    /// Implicit operator.
    let inline (!>) (x:^a) : ^b = ((^a or ^b) : (static member op_Implicit : ^a -> ^b) x)


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Result =

    [<CompiledName("Map")>]
    let map (mapping: 'a -> 'b) (result: Result<'a, 'e>) : Result<'b, 'e> =
        match result with
        | Ok x -> Ok (mapping x)
        | Error e -> Error e

    [<CompiledName("MapError")>]
    let mapError (mapping: 'e -> 'f) (result: Result<'a, 'e>) : Result<'a, 'f> =
        match result with
        | Ok x -> Ok x
        | Error e -> Error (mapping e)

    [<CompiledName("Bind")>]
    let bind (binder: 'a -> Result<'b, 'e>) (result: Result<'a, 'e>) : Result<'b, 'e> =
        match result with
        | Ok x -> binder x
        | Error e -> Error e

    let zipWith (f: 'e -> 'e -> 'e) (x: Result<'a, 'e>) (y: Result<'b, 'e>) : Result<('a * 'b), 'e> =
        match x, y with
        | Ok a, Ok b -> Ok (a, b)
        | Ok _, Error e -> Error e
        | Error e, Ok _ -> Error e
        | Error e1, Error e2 -> Error (f e1 e2)


module Option =

    /// Given a default value and an option, returns the option value if there else the default value.
    let inline isNull defaultValue = function Some v -> v | None -> defaultValue


module Text =
    open System.Text

    let UTF8 = Encoding.UTF8

    let UTF8NoBOM = new UTF8Encoding(false, true)


[<AutoOpen>]
module ArraySegmentExtensions =
    open System
    open System.Text
    open System.IO

    type Encoding with
        member x.GetString(data:ArraySegment<byte>) = x.GetString(data.Array, data.Offset, data.Count)


[<AutoOpen>]
module AsyncExtensions =
    open System
    open System.Threading
    open System.Threading.Tasks

    type Async with

        /// An async computation which does nothing and completes immediately.
        static member inline empty = async.Return()

        /// An async computation which does nothing and never completes.
        static member inline never : Async<unit> = Async.Sleep Timeout.Infinite

        static member map (f:'a -> 'b) (a:Async<'a>) : Async<'b> = async.Bind(a, f >> async.Return)

        static member inline bind (f:'a -> Async<'b>) (a:Async<'a>) : Async<'b> = async.Bind(a, f)

        static member inline join (a:Async<Async<'a>>) : Async<'a> = Async.bind id a

        static member AwaitTaskCorrect(task : Task) : Async<unit> =
            Async.FromContinuations(fun (sc,ec,cc) ->
                task.ContinueWith(fun (task:Task) ->
                    if task.IsFaulted then
                        let e = task.Exception
                        if e.InnerExceptions.Count = 1 then ec e.InnerExceptions.[0]
                        else ec e
                    elif task.IsCanceled then
                        ec(TaskCanceledException())
                    else
                        sc ())
                |> ignore)

        static member AwaitTaskCorrect(task : Task<'T>) : Async<'T> =
            Async.FromContinuations(fun (sc,ec,cc) ->
                task.ContinueWith(fun (task:Task<'T>) ->
                    if task.IsFaulted then
                        let e = task.Exception
                        if e.InnerExceptions.Count = 1 then ec e.InnerExceptions.[0]
                        else ec e
                    elif task.IsCanceled then
                        ec(TaskCanceledException())
                    else
                        sc task.Result)
                |> ignore)


module Map =

  /// Gets the set of keys.
  let inline keys m = m |> Map.toSeq |> Seq.map fst |> Set.ofSeq

  /// Merges two maps favoring values from the second.
  let merge (a:Map<'a, 'b>) (b:Map<'a, 'b>) =
    Set.union (keys a) (keys b)
    |> Seq.choose (fun k ->
      match (a |> Map.tryFind k), (b |> Map.tryFind k) with
      | _, Some b -> Some (k,b)
      | Some a, _ -> Some (k,a)
      | _ -> None
    )
    |> Map.ofSeq
