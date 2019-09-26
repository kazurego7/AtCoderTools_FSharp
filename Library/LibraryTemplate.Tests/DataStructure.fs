module DataStructure

open AtCoder.DataStructure
open Expecto
open System
open TestTools
open UnionFind

[<Tests>]
let tests =
    testList "DataStructure" [
        testList "UnionFind" [
            test "ok" {
                let ids = [ (0, 3); (1, 2); (0, 2) ]
                let uf = UnionFind 5
                for (u, v) in ids do
                    uf.Unite u v
                let sizes = seq { 0..4 } |> Seq.map uf.Size
                Expect.sequenceContainsOrder sizes (seq { yield 4; yield 4; yield 4; yield 4; yield 1; }) ""
            }
            testList "Errors" [
                let uf = UnionFind 3
                yield testList "Unite" [
                    testError (fun () -> uf.Unite 3 0) "u is equal or over n"
                    testError (fun () -> uf.Unite 0 3) "v is equal or over n"
                ]
                yield testList "Size" [
                    testError (fun () -> uf.Size 3) "u is equal or over n"
                ]
                yield testList "Find" [
                    testError (fun () -> uf.Find 3 0) "u is equal or over n"
                    testError (fun () -> uf.Find 0 3) "v is equal or over n"
                ]
            ]
        ]
    ]
