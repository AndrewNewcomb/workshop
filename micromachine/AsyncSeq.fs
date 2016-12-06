namespace Micromachine

module AsyncSeq =
    open FSharp.Control
    open System.Threading
    
    let readAhead max xs : AsyncSeq<'a> =
        if max < 1 then invalidArg "max" "must be positive"
        let mutable count = 0
        let buffer = Array.create max None
        let resetR = new AutoResetEvent(false)
        let resetW = new AutoResetEvent(false)

        let rec read head = async {
            let c = count // read the outer value once (32bit atomic)
            if c = 0 then
                do! Async.AwaitWaitHandle(resetR) |> Async.Ignore
                return! read head
            else
                let x = buffer.[head]
                buffer.[head] <- None
                let c' = Interlocked.Decrement(&count)
                if c' = max - 1 then resetW.Set() |> ignore
                return x |> Option.map (fun v -> v, (head + 1) % max)
        }

        let rec write tail x = async {
            let c = count // Atomic read of 32bit value
            if c = max then
                do! Async.AwaitWaitHandle(resetW) |> Async.Ignore
                return! write tail x
            else
                buffer.[tail] <- Some x
                let c' = Interlocked.Increment(&count)
                if c' = 1 then resetR.Set() |> ignore
                return (tail + 1) % max
        }

        async {
            let! tail = xs |> AsyncSeq.foldAsync write 0
            // tell it to read one None to terminate the read-side
            if Interlocked.Increment(&count) = 1 then
                resetR.Set() |> ignore
        } |> Async.Start

        AsyncSeq.unfoldAsync read 0
