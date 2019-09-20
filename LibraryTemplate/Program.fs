open System

module BasicLibrary =
    let interval startInclusive endExclusive =
        seq { startInclusive..(endExclusive - 1) } |> Seq.toList

open BasicLibrary

module AtCoderLibrary =
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
        let powMod b n divisor =
            List.replicate b n
            |> Seq.fold (fun accm i -> accm * i % divisor) 1L
            |> int32

open AtCoderLibrary
open InputOutputs
open NumericFunctions

[<EntryPoint>]
let main _ = 0 // return an integer exit code
