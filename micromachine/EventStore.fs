namespace Micromachine

module EventStore =
    open FSharp.Control
    open FSharp.Data
    open EventStore.ClientAPI
    open System
    open System.Net
    open Micromachine

    type Connection = IEventStoreConnection

    // Events -----------------------------------------------------------------

    type Event<'a> =
      { eventId : Guid
        eventType : string
        eventStream : string
        eventNumber : int
        data : 'a }

    type SerializedEvent = Event<SerializedPayload>
    and SerializedPayload = Raw of byte[] | Json of byte[]

    type JsonEvent = Event<JsonValue>

    [<RequireQualifiedAccess>]
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Event =

        let ofESRecordedEvent (parse:SerializedPayload -> 'a) (re:RecordedEvent) =
          { eventId = re.EventId
            eventType = re.EventType
            eventStream = re.EventStreamId
            eventNumber = re.EventNumber
            data = parse <| if re.IsJson then Json(re.Data) else Raw(re.Data) }

        let ofESResolvedEvent (parse:SerializedPayload -> 'a) (re:ResolvedEvent) =
            ofESRecordedEvent parse (re.OriginalEvent)

        let map f e =
          { eventId = e.eventId
            eventType = e.eventType
            eventStream = e.eventStream
            eventNumber = e.eventNumber
            data = f e.data }

        let mapResult f e =
            f e.data |> Result.map(fun data' ->
              { eventId = e.eventId
                eventType = e.eventType
                eventStream = e.eventStream
                eventNumber = e.eventNumber
                data = data' })

    // Base API ---------------------------------------------------------------

    let connect connectTo : Async<Connection> =
        let connectionString = sprintf "ConnectTo=%s" connectTo
        let conn = EventStoreConnection.Create(connectionString)
        conn.ConnectAsync() |> Async.AwaitTask |> Async.map(fun _ -> conn)
