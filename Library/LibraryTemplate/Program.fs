module AtCoder

open System
open Microsoft.FSharp.Collections

module InputOutputs =
    let read() = Console.ReadLine()
    let reads() = read().Split()

    let readMatrix() =
        let mutable lines = []
        let line = reads() |> Array.toSeq
        while not (Seq.isEmpty line) do
            lines <- line :: lines
        lines
        |> List.rev
        |> List.toSeq
        |> array2D

    let inline printRow list =
        let strs = list |> List.map string
        if List.isEmpty strs then
            printf "%s" strs.[0]
            for s in List.skip 1 strs do
                printf " %s" s
        printf "\n"

module NumericFunctions =
    type Mod =
        { divisor : int32 }

        member this.Pow (b : int64) (n : int32) : int32 =
            List.replicate n b
            |> List.fold (fun product i -> product * i % int64 this.divisor) 1L
            |> int32

        member this.Perm (n : int32) (k : int32) : int32 =
            match (n, k) with
            | (n, _) when n < 0 -> invalidArg "n" "n >= 0"
            | (_, k) when k < 0 -> invalidArg "k" "k >= 0"
            | (n, k) when k > n -> 0
            | _ ->
                [ n - k + 1..n ]
                |> List.fold
                       (fun product m -> product * int64 m % int64 this.divisor)
                       1L
                |> int32

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
                    match n, k with
                    | n, k when n < k -> table.[n, k] <- -1
                    | _, k when k = 0 -> table.[n, k] <- 1
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
