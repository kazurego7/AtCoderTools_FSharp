module AtCoder

open Microsoft.FSharp.Collections
open System


module Seq =
    let interval startInclusive endExclusive =
        seq { startInclusive..(endExclusive - 1) }

module InputOutputs =
    let read() : string =
        Console.ReadLine()

    let reads() : string [] =
        read().Split()

    let readMatrix() : string [,] =
        let mutable lines = []
        let line = reads() |> Array.toSeq
        while not (Seq.isEmpty line) do
            lines <- line :: lines
        lines
        |> Seq.rev
        |> array2D

    let readInt32() : int32 =
        read() |> int32

    let readInt64() : int64 =
        read() |> int64

    let inline int32s (source : seq<'a>) : seq<int32> =
        Seq.map int32 source

    let inline int64s (source : seq<'a>) : seq<int64> =
        Seq.map int64 source

    let readInt32s() : seq<int32> =
        reads() |> int32s

    let readInt64s() : seq<int64> =
        reads() |> int64s

    let inline print (item : 'a) : unit =
        printfn "%A" item

    let inline printRow (line : seq<'a>) : unit =
        let strs = line |> Seq.map string
        if Seq.isEmpty strs then
            printf "%s" (Seq.head strs)
            for s in Seq.skip 1 strs do
                printf " %s" s
        printf "\n"

    let inline printColumn (line : seq<'a>) : unit =
        for item in line do
            printf "%A" item

    let inline printGridGraph (lines : 'a [,]) : unit =
        for i in (Seq.interval 0 lines.Length) do
            lines.[i, *]
                |> Seq.map string
                |> String.concat " "
                |> print


module NumericFunctions =
    type Mods =
        { divisor : int32 }
        member this.Mod(a : int64) =
            let b = a % int64 this.divisor |> int32
            if b < 0 then b + this.divisor else b

        member this.Mod(a : int32) =
            this.Mod(int64 a)

        member this.Add (a : int32) (b : int32) : int32 =
            (this.Mod a + this.Mod b) % this.divisor

        member this.Sub (a : int32) (b : int32) : int32 =
            let sub = (this.Mod a - this.Mod b) % this.divisor
            if sub < 0 then sub + this.divisor else sub

        member this.Mul (a : int32) (b : int32) : int32 =
            (int64 (this.Mod a) * int64 (this.Mod b)) % int64 this.divisor |> int32

        member this.Pow (b : int32) (n : int32) : int32 =
            let digit = int32 (Math.Log(double n, 2.0))
            let seqs = seq { 0..digit }
                        |> Seq.scan (fun acm _ -> this.Mul acm acm) b
                        |> Seq.toArray
            seq { 0..digit }
                |> Seq.fold (fun acm i ->
                    if ((n >>> i) &&& 1) = 1 then
                        this.Mul acm seqs.[i]
                    else
                        acm
                    ) 1
        member this.Inv(a : int32) : int32 =
            this.Pow a (this.divisor - 2)

        member this.Div (a : int32) (b : int32) : int32 =
            this.Mul a (this.Inv b)

        member this.Perm (n : int32) (k : int32) : int32 =
            match (n, k) with
            | (n, _) when n < 0 -> invalidArg "n" "n >= 0"
            | (_, k) when k < 0 -> invalidArg "k" "k >= 0"
            | (n, k) when k > n -> 0
            | _ ->
                seq { n - k + 1..n }
                |> Seq.fold this.Mul 1

        member this.FactTable(nMax : int32) : int32 [] =
            seq { 1..nMax }
            |> Seq.scan this.Mul 1
            |> Seq.toArray

        member this.CombTable(nMax : int32) : int32 [,] =
            let table = Array2D.zeroCreate (nMax + 1) (nMax + 1)
            for n in 0..nMax do
                for k in 0..nMax do
                    match (n, k) with
                    | (n, k) when n < k -> table.[n, k] <- 0
                    | (_, k) when k = 0 -> table.[n, k] <- 1
                    | _ ->
                        table.[n, k] <- int64 table.[n - 1, k - 1]
                                        + int64 table.[n - 1, k]
                                          % int64 this.divisor |> int32
            table

    module Int64 =
        let isEven (a : int64) : bool =
            a % 2L = 0L
        let isOdd (a : int64) : bool =
            not (isEven a)
    module Int32 =
        let isEven (a : int32) : bool =
            Int64.isEven (int64 a)
        let isOdd (a : int32) : bool =
            Int64.isOdd (int64 a)

open InputOutputs
open NumericFunctions

[<EntryPoint>]
let main _ = 0 // return an integer exit code
