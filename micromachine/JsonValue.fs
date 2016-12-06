namespace Micromachine

open System
open System.Globalization
open System.Text
open System.IO

open FSharp.Data
open FSharp.Data.JsonExtensions

type ToJsonClass = ToJsonClass

type FromJsonClass = FromJsonClass

/// JSON parse result - either a parsed value or an error message.
type JsonResult<'a> = Result<'a, string>

/// A field of a JSON object.
type JsonField = string * JsonValue

exception JsonException of string

[<AutoOpen>]
module JsonValueMappingEx =

    type JsonValue with

        static member toString (json:JsonValue, saveOptions) =
            let w = new StringWriter(CultureInfo.InvariantCulture)
            json.WriteTo(w, saveOptions)
            w.GetStringBuilder().ToString()

        static member toBytes (json:JsonValue) =
          (use ms = new MemoryStream()
          (use w = new StreamWriter(ms)
           json.WriteTo(w, JsonSaveOptions.DisableFormatting)
           w.Flush()
           ms.ToArray()))

        static member toString (json:JsonValue) = JsonValue.toString (json,JsonSaveOptions.None)

        static member toStringNoFormat (json:JsonValue) = JsonValue.toString (json,JsonSaveOptions.DisableFormatting)

        static member Parse (data:byte[]) = JsonValue.Parse(Text.UTF8NoBOM.GetString(data))

        static member Parse (data:ArraySegment<byte>) = JsonValue.Parse(Text.UTF8NoBOM.GetString(data))


    /// A partial active pattern which matches a JsonValue.Record with a single property of a specified name.
    let (|OneJsonMember|_|) (name:string) = function
        | JsonValue.Record props when props.Length = 1 ->
            let key,value = props.[0]
            if key.Equals(name, StringComparison.OrdinalIgnoreCase) then Some value
            else None
        | _ -> None

    /// An active pattern which matches a JsonValue.Record having a property of the specified name
    /// and allowing for additional fields before and after.
    let (|JsonRecordField|_|) (name:string) = function
        | JsonValue.Record props ->
            props
            |> Array.tryFind (fun (n,_) -> n = name)
            |> Option.map (fun (_,p) -> p)
        | _ -> None

    /// An active pattern which matches a JsonValue.Record having exactly one property property with the specified name.
    let (|JsonRecordFieldExact|_|) (name:string) = function
        | JsonValue.Record [| name, json |] -> Some json
        | _ -> None


/// Operations on JsonResult.
module JsonResult =

    let inline unit a : JsonResult<'a> = Ok a

    let inline error err : JsonResult<'a> = Error err

    /// Gets the result of a successful parse or throws.
    let get<'a> (r:JsonResult<'a>) : 'a =
        match r with
        | Ok r -> r
        | Error err -> raise (JsonException (sprintf "JSON parse failed = %s" err))

    let getOr<'a> (defaultValue:'a) (r:JsonResult<'a option>) : 'a =
        match r with
        | Ok r -> r |> Option.isNull defaultValue
        | Error _ -> defaultValue

    let tryGet<'a> (r:JsonResult<'a>) : 'a option =
        match r with
        | Ok r -> Some r
        | _ -> None

    let tryGetOpt<'a> (r:JsonResult<'a option>) : 'a option =
        match r with
        | Ok r -> r
        | _ -> None

    let map f (r:JsonResult<'a>) : JsonResult<'b> =
        match r with
        | Ok a -> unit (f a)
        | Error err -> error err

    let mapError f (r:JsonResult<'a>) : JsonResult<'a> =
        match r with
        | Ok a -> unit a
        | Error err -> error (f err)

    let bind f (r:JsonResult<'a>) : JsonResult<'b> =
        match r with
        | Ok a -> f a
        | Error err -> error err

    let traverseA (f:'a -> JsonResult<'b>) (xs:'a[]) : JsonResult<'b[]> =
        let length = Array.length xs
        let acc : 'b[] = Array.zeroCreate length
        let rec go i =
            if i = length then null
            else
                match f xs.[i] with
                | Ok a ->
                    acc.[i] <- a
                    go (i + 1)
                | Error err -> err
        let err = go 0
        if err = null then unit acc
        else error err

    type JsonParserBuilder() =
        member __.Return a = unit a
        member __.ReturnFrom r = r
        member __.Bind (inp:JsonResult<'a>, body:'a -> JsonResult<'b>) : JsonResult<'b> = bind body inp


/// JSON parse result - either a parsed value or an error message.
type ParseResult<'a> = Choice<'a, string>

/// Operations on JsonResult.
module ParseResult =

    let inline unit a : JsonResult<'a> = Ok a

    let inline error err : JsonResult<'a> = Error err

    /// Gets the result of a successful parse or throws.
    let get<'a> (r:JsonResult<'a>) : 'a =
        match r with
        | Ok r -> r
        | Error err -> raise (JsonException (sprintf "JSON parse failed = %s" err))

    let getOr<'a> (defaultValue:'a) (r:JsonResult<'a option>) : 'a =
        match r with
        | Ok r -> r |> Option.isNull defaultValue
        | Error _ -> defaultValue

    let tryGet<'a> (r:JsonResult<'a>) : 'a option =
        match r with
        | Ok r -> Some r
        | _ -> None

    let tryGetOpt<'a> (r:JsonResult<'a option>) : 'a option =
        match r with
        | Ok r -> r
        | _ -> None

    let map f (r:JsonResult<'a>) : JsonResult<'b> =
        match r with
        | Ok a -> unit (f a)
        | Error err -> error err

    let mapError f (r:JsonResult<'a>) : JsonResult<'a> =
        match r with
        | Ok a -> unit a
        | Error err -> error (f err)

    let bind f (r:JsonResult<'a>) : JsonResult<'b> =
        match r with
        | Ok a -> f a
        | Error err -> error err

    let traverseA (f:'a -> JsonResult<'b>) (xs:'a[]) : JsonResult<'b[]> =
        let length = Array.length xs
        let acc : 'b[] = Array.zeroCreate length
        let rec go i =
            if i = length then null
            else
                match f xs.[i] with
                | Ok a ->
                    acc.[i] <- a
                    go (i + 1)
                | Error err -> err
        let err = go 0
        if err = null then unit acc
        else error err


[<AutoOpen>]
module Helpers =

    let jsonParse = new JsonResult.JsonParserBuilder()

    let inline toJson_ (a: ^a, b: ^b) = ((^a or ^b) : (static member ToJson: ^b -> JsonValue) b)

    /// Encodes a value of a type containing a ToJson function into a JsonValue.
    let inline toJson x : JsonValue = toJson_ (ToJsonClass, x)

    /// Encodes a value into a JSON string.
    let inline toJsonString x : string = x |> toJson |> JsonValue.toString

    /// Encodes a value into a JSON byte array.
    let inline toJsonBytes x : byte[] = x |> toJson |> JsonValue.toString |> Encoding.UTF8.GetBytes

    /// Creates a JSON property.
    let inline jprop (name:string) (x:'a) = (name,toJson x)

    /// Creates a JSON object.
    let inline jobj (props:(string * JsonValue)[]) = props |> JsonValue.Record

    /// Creates a successful JsonResult.
    let succeed x = JsonResult.unit x

    /// Creates a failed JsonResult.
    let fail err = JsonResult.error err

    let failOn (expectedType:string) (json:JsonValue) =
        JsonResult.error (sprintf "Expected '%s' but was '%A'" expectedType json)

    let inline private fromJson_ (a:^a, b:^b) = ((^a or ^b) : (static member FromJson:^b -> (JsonValue -> ^b JsonResult)) b)

    /// Parses a JsonValue into a specified type containing a FromJson function.
    let inline fromJson (json:JsonValue) =
        fromJson_ (FromJsonClass, Unchecked.defaultof<'a>) json

    /// Parse a JsonValue into a specified type and applies a mapping.
    let inline fromJsonTo (json:JsonValue) (f:'a -> 'b) =
        fromJson json |> JsonResult.map f

    /// Parses a JSON string.
    let inline parseJson (json:string) =
        JsonValue.Parse json |> fromJson

    /// Parses a byte array.
    let inline parseJsonBytes (json:byte[]) =
        Encoding.UTF8.GetString(json) |> JsonValue.Parse |> fromJson

    /// Parses a JSON string to 'T option
    let inline tryParseJson (json:string) =
        json |> parseJson |> ParseResult.tryGet

    /// Parses a byte array to 'T option
    let inline tryParseBytes (json:byte[]) =
        json |> parseJsonBytes |> ParseResult.tryGet

    /// Parses a JSON property into a specified type.
    let inline jget (json:JsonValue) key : JsonResult<'a> =
        match json.TryGetProperty key with
        | Some prop -> fromJson prop
        | None -> fromJson JsonValue.Null

    /// Optionally parses a JSON property into a specified type.
    let inline jgetopt (json:JsonValue) key : JsonResult<'a option> =
        match json.TryGetProperty key with
        | Some prop ->
            match prop with
            | JsonValue.Null -> succeed None
            | prop -> fromJson prop |> JsonResult.map Some
        | None -> succeed None

    let parseObj (f:JsonValue -> JsonResult<'a>) (json:JsonValue) : JsonResult<'a> =
        match json with
        | JsonValue.Record _ -> f json
        | _ -> failOn "JsonValue.Record" json

    /// Merges two Json records, favoring fields from the second when there is a match.
    /// Arrays are just concatenated.
    let jmerge (a:JsonValue) (b:JsonValue) =
        match a,b with
        | JsonValue.Record a, JsonValue.Record b -> Map.merge (Map.ofArray a) (Map.ofArray b) |> Map.toArray |> JsonValue.Record
        | JsonValue.Array a, JsonValue.Array b -> JsonValue.Array (Array.append a b)
        | JsonValue.Null, JsonValue.Array _ -> b
        | JsonValue.Null, JsonValue.Record _ -> b
        | JsonValue.Array _, JsonValue.Null -> a
        | JsonValue.Record _, JsonValue.Null -> a
        | _ -> failwith "can only merge JsonValue.Record or JsonValue.Array values!"

    let inline toJsonField (x: ^a) : JsonField =
        (^a : (static member ToJsonField: ^a -> JsonField) x)

    let inline fromJsonField (f:JsonField) =
        (^a : (static member FromJsonField:JsonField -> ^a JsonResult) f)

    /// Creates a function which encodes a value of type 'a as a record of fields of type 'b.
    let inline toJsonChild (f:'a -> 'b[]) (a:'a) : JsonValue =
        f a |> Array.map toJsonField |> JsonValue.Record

    /// Creates a function which decodes a JSON record into an object based on decoding individual fields.
    let inline fromJsonChild (f:seq<'b> -> 'a) = function
        | JsonValue.Record fields -> fields |> JsonResult.traverseA fromJsonField |> JsonResult.map f
        | json -> failOn "JsonValue.Record" json


type ToJsonClass with
    static member inline ToJson (x:JsonValue) = x
    static member inline ToJson (x:(string * JsonValue)[]) = JsonValue.Record x
    static member inline ToJson (x:JsonValue[]) = JsonValue.Array x
    static member inline ToJson (x:int) = JsonValue.Number (decimal x)
    static member inline ToJson (x:int64) = JsonValue.Number (decimal x)
    static member inline ToJson (x:int16) = JsonValue.Number (decimal x)
    static member inline ToJson (x:bool) = JsonValue.Boolean x
    static member inline ToJson (x:float) = JsonValue.Float x
    static member inline ToJson (x:single) = JsonValue.Float (float x)
    static member inline ToJson (x:char) = JsonValue.String (x.ToString())
    static member inline ToJson (x:decimal) = JsonValue.Number x
    static member inline ToJson (x:string) = if x = null then JsonValue.Null else JsonValue.String x
    static member inline ToJson (x:DateTime) = JsonValue.String (x.ToString("yyyy-MM-ddTHH:mm:ssZ"))
    static member inline ToJson (x:DateTimeOffset) = JsonValue.String (x.ToString("yyyy-MM-ddTHH:mm:ssK"))

type ToJsonClass with
    static member inline ToJson (x:'a array) = x |> Array.map toJson |> JsonValue.Array
    //static member inline ToJson (x:System.Collections.Generic.IEnumerable<'a>) = x |> Seq.toArray |> ToJsonClass.ToJson
    static member inline ToJson (x:'a list) = x |> List.toArray |> ToJsonClass.ToJson
    static member inline ToJson (x:'a Set) = x |> Set.toArray |> ToJsonClass.ToJson
    static member inline ToJson (x:(string * _) list) = x |> Seq.map (fun (k,v) -> k,toJson v) |> Seq.toArray |> JsonValue.Record
    static member inline ToJson (x:(string * _) array) = x |> Array.map (fun (k,v) -> k,toJson v) |> JsonValue.Record
    static member inline ToJson (x:(string * _) Set) = x |> Set.map (fun (k,v) -> k,toJson v) |> Set.toArray |> JsonValue.Record
    static member inline ToJson (x:Map<string, 'a>) = x |> Map.map (fun _ v -> toJson v) |> Map.toArray |> jobj
    static member inline ToJson (x:'a option) = match x with Some x -> toJson x | None -> JsonValue.Null


type ToJsonClass with
    static member inline ToJson (x:Uri) = if x <> null then JsonValue.String (x.AbsoluteUri) else JsonValue.Null


type FromJsonClass with

    static member FromJson (_:JsonValue) = fun (json:JsonValue) -> json |> succeed

    static member FromJson (_:string) = function
        | JsonValue.String s -> succeed s
        | JsonValue.Null -> succeed null
        | json -> failOn "string" json

    static member FromJson (_:int) = function
        | JsonValue.Number _ as j -> j.AsInteger() |> succeed
        | JsonValue.Float _ as j -> j.AsInteger() |> succeed
        | json -> failOn "int" json

    static member FromJson (_:int64) = function
        | JsonValue.Number _ as j -> j.AsInteger64() |> succeed
        | JsonValue.Float _ as j -> j.AsInteger64() |> succeed
        | json -> failOn "int64" json

    static member FromJson (_:decimal) = function
        | JsonValue.Number _ as j -> j.AsDecimal() |> succeed
        | JsonValue.Float _ as j -> j.AsDecimal() |> succeed
        | json -> failOn "decimal" json

    static member FromJson (_:float) = function
        | JsonValue.Number _ as j -> j.AsFloat() |> succeed
        | JsonValue.Float _ as j -> j.AsFloat() |> succeed
        | json -> failOn "float" json

    static member FromJson (_:bool) = function
        | JsonValue.Boolean b -> b |> succeed
        | json -> failOn "bool" json

    static member FromJson (_:DateTime) = function
        | JsonValue.String s ->
            match DateTime.TryParse(s) with
            | true,dt -> succeed dt
            | _ -> fail "datetime"
        | json -> failOn "datetime" json

    static member FromJson (_:DateTimeOffset) = function
        | JsonValue.String s ->
            match DateTimeOffset.TryParse(s) with
            | true,dt -> succeed dt
            | _ -> fail "datetime"
        | json -> failOn "datetime" json

    static member inline FromJson (_:'a option) = function
        | JsonValue.Null -> succeed None
        | json ->
            let a : JsonResult<'a> = fromJson json
            a |> JsonResult.map Some

    static member inline FromJson (_:'a[]) = function
        | JsonValue.Array xs ->
            let xs : JsonResult<'a[]> = xs |> JsonResult.traverseA fromJson
            xs
        | JsonValue.Null -> Array.empty |> JsonResult.unit
        | _ -> fail "array"

    static member inline FromJson (_:list<'a>) = function
        | JsonValue.Array xs ->
            let xs : JsonResult<'a[]> = xs |> JsonResult.traverseA fromJson
            xs |> JsonResult.map List.ofArray
        | JsonValue.Null -> List.empty |> JsonResult.unit
        | json -> failOn "list" json

    static member inline FromJson (_:Set<'a>) = function
        | JsonValue.Array xs ->
            let xs : JsonResult<'a[]> = xs |> JsonResult.traverseA fromJson
            xs |> JsonResult.map Set.ofArray
        | JsonValue.Null -> Set.empty |> JsonResult.unit
        | json -> failOn "set" json

    static member inline FromJson (_:Map<string, 'a>) = function
        | JsonValue.Record xs ->
            let xs : JsonResult<(string * 'a)[]> = xs |> JsonResult.traverseA (fun (k,v) -> fromJson v |> JsonResult.map (fun v -> k,v))
            xs |> JsonResult.map (Map.ofArray)
        | JsonValue.Null -> Map.empty<string, 'a> |> JsonResult.unit
        | json -> failOn "map" json


[<AutoOpen>]
module Ops =

    /// Creates a JSON property given a key (property name) and corresponding value.
    let inline (.=) key value = jprop key value

    /// Parses a property from a JSON value.
    let inline (.@) json key = jget json key |> JsonResult.mapError (fun err -> String.Concat [| key; ": "; err|])

    /// Parses a property from a JSON value, which may be absent.
    let inline (.@?) json key = jgetopt json key

    /// Parses a property from a JSON value, which may be absent.
    let inline (.@>) json key = jgetopt json key |> JsonResult.tryGetOpt

    /// Merges two Json arrays or records.
    let inline (++) a b = jmerge a b


/// A codec for JsonValue.
type JsonValueCodec<'a> = Codec<JsonValue, 'a>

/// Operations on JsonValue codecs.
module JsonValueCodec =

  let inline jsonValueCodecT () : JsonValueCodec<_> =
    (fromJson >> JsonResult.tryGet, toJson)

  /// Maps a JsonValue codec to a byte array codec.
  let inline toBytes (c:JsonValueCodec<'a>) : ByteCodec<'a> =
    c |> Codec.dimap (JsonValue.Parse) (JsonValue.toBytes)
