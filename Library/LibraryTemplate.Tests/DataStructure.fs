module DataStructure

open AtCoder.DataStructure
open Expecto
open PriorityQueue
open System
open TestTools

[<Tests>]
let tests =
    testList "DataStructure"
        [ testList "UnionFind"
              [ test "ok" {
                    let ids =
                        [ (0, 3)
                          (1, 2)
                          (0, 2) ]

                    let uf = UnionFind 5
                    // (0 1 2 3) (4)
                    for (u, v) in ids do
                        uf.Unite (Id u) (Id v)
                    let sizes = seq { 0 .. 4 } |> Seq.map (Id >> uf.Size)
                    Expect.sequenceContainsOrder sizes
                        (seq {
                            yield 4
                            yield 4
                            yield 4
                            yield 4
                            yield 1
                         }) ""
                }
                testList "Errors"
                    [ let uf = UnionFind 3
                      yield testList "Unite"
                                [ testError (fun () -> uf.Unite (Id 3) (Id 0)) "u is equal or over n"
                                  testError (fun () -> uf.Unite (Id 0) (Id 3)) "v is equal or over n" ]
                      yield testList "Size" [ testError (fun () -> uf.Size(Id 3)) "u is equal or over n" ]
                      yield testList "Find"
                                [ testError (fun () -> uf.Find (Id 3) (Id 0)) "u is equal or over n"
                                  testError (fun () -> uf.Find (Id 0) (Id 3)) "v is equal or over n" ] ] ]
          testList "reverseCompare" [ testValueEqual (reverseCompare 1 3) 1 "x < y -> x > y" ]
          testList "PriortyQueue"
              [ test "ok" {
                    let first = seq [ 1; 4; 3; 2; 3; 3 ]
                    let pq = PriorityQueue(first, reverseCompare)
                    Expect.equal pq.Peek 4 "peek 4"
                    Expect.equal pq.Size 4 "size 4"
                    pq.Enqueue 5 |> ignore
                    Expect.equal pq.Peek 5 "enqueue 5"
                    let five = pq.Dequeue()
                    Expect.equal five 5 "dequeue 5"
                } ] ]
