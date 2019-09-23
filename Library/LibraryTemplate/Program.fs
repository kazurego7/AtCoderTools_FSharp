module AtCoder

open Microsoft.FSharp.Collections
open System

module InputOutputs =
    let read() = Console.ReadLine()
    let reads() = read().Split()

    let readMatrix() =
        let mutable lines = []
        let line = reads() |> Array.toSeq
        while not (Seq.isEmpty line) do
            lines <- line :: lines
        lines
        |> Seq.rev
        |> array2D

    let inline printRow line =
        let strs = line |> Seq.map string
        if Seq.isEmpty strs then
            printf "%s" (Seq.head strs)
            for s in Seq.skip 1 strs do
                printf " %s" s
        printf "\n"

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
            Seq.replicate n b
            |> Seq.fold this.Mul 1

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

        member this.Fact(n : int32) : int32 = this.Perm n n

        member this.FactTable(nMax : int32) =
            [ 1..nMax ]
            |> List.scan
                   (fun product i -> product * int64 i % int64 this.divisor) 1L
            |> List.map int32
            |> List.toArray

        member this.CombTable (nMax : int32) (kMax : int32) =
            let table = Array2D.zeroCreate nMax kMax
            for n in 0..nMax do
                for k in 0..kMax do
                    match (n, k) with
                    | (n, k) when n < k -> table.[n, k] <- -1
                    | (_, k) when k = 0 -> table.[n, k] <- 1
                    | _ ->
                        table.[n, k] <- int64 table.[n - 1, k - 1]
                                        + int64 table.[n - 1, k]
                                          % int64 this.divisor |> int32
            table

    module List =
        let interval startInclusive endExclusive =
            seq { startInclusive..(endExclusive - 1) } |> Seq.toList

open InputOutputs
open NumericFunctions

[<EntryPoint>]
let main _ = 0 // return an integer exit code
