open System

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

[<EntryPoint>]
let main _ = 0 // return an integer exit code
