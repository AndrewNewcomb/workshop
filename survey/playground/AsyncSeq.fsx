#load "../refs.fsx"

// Load our unquote library directly from the packages folder. This provides us
// with the `test` function used on the quotations below.
#r "../../packages/test/Unquote/lib/net45/Unquote.dll"
open Swensen.Unquote

// We're going to be using FSharp.Control.AsyncSeq in this
// playground script.
open FSharp.Control

// Sequence Expressions -------------------------------------------------------

// fold over a collection like [1; 2; 3] is like replacing the ;'s with your
// function. So if we pass (+), it'd be like (1 + 2 + 3) or 6. We can use
// any function which takes the state and then the value and returns the new
// state. The final state when the sequence is ended will be our value.

async {

    let simpleSeq = seq {yield 1; yield 2; yield 3}
    let seqSum = simpleSeq |> Seq.fold (fun sum n -> sum + n) 0

    let simpleASeq = asyncSeq {yield 1; yield 2; yield 3}
    // Note that the result from the fold is an Async<int> so we use `let!`
    // There is also a foldAsync alternative.
    let! aseqSum = simpleASeq |> AsyncSeq.fold (fun sum n -> sum + n) 0

    // These samples are roughly equivalent and should give the same result:
    test <@ seqSum = aseqSum @>

} |> Async.RunSynchronously


// Unfolding Sequences --------------------------------------------------------

// Unfolding is an alternative way to generate a sequence. Compare these two
// count-down sequences:

let rec countDownExp n = asyncSeq {
    if n = 0
    then return ()
    else
        yield n
        yield! countDownExp (n - 1) 
}

// Note that the recursion itself is encoded with unfold so there is no
// need for `let rec`.
let countDownUnfold n =
    let step n = if n = 0 then None else Some(n, n - 1)
    AsyncSeq.unfold step n

async {
    let! countdownA = countDownExp 10 |> AsyncSeq.toArrayAsync
    let! countdownB = countDownUnfold 10 |> AsyncSeq.toArrayAsync
    test <@ countdownA = countdownB @>
} |> Async.RunSynchronously

// The unfoldAsync variant can also be useful:
let countdown n =
    let step n = async {
        do! Async.Sleep 1000
        if n = 0
        then return None
        else return Some(n, n - 1)
    }
    AsyncSeq.unfoldAsync step n

// More on iter below.
countdown 10 |> AsyncSeq.iter (printfn "%i") |> Async.RunSynchronously


// AsyncSeq.map ---------------------------------------------------------------

// mapping is quite useful when we want to adjust the type of the element in
// the sequence

let xs =
    asyncSeq {yield 1; yield 2; yield 3}
    |> AsyncSeq.map (sprintf "(%i)")

async {
    let! array = xs |> AsyncSeq.toArrayAsync
    test <@ array = [|"(1)"; "(2)"; "(3)"|] @>
} |> Async.RunSynchronously


// AsyncSeq.iter --------------------------------------------------------------

// Sometimes we simply want to entirely consume our sequence. We can use iter
// for this kind of effectful iteration over each element.

let noisy = asyncSeq {
    printfn "about to yield 1"
    yield 1
    printfn "about to yield 2"
    yield 2
    printfn "about to yield 3"
    yield 3
}

async {
    do! noisy |> AsyncSeq.iter (printfn "got %i")
} |> Async.RunSynchronously

// There is also iterAsync:

async {
    do! noisy |> AsyncSeq.iterAsync (fun x -> async {
        do! Async.Sleep 1000
        printfn "got %i" x
    })
} |> Async.RunSynchronously
