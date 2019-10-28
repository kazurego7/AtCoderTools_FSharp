module AtCoder.Algorithm.Test

open AtCoder.Algorithm
open Expecto
open System
open TestTools

[<Tests>]
let tests =
    testList "Algorithm"
        [ testList "binarySearch"
              [ test "empty" {
                    let arr = [||]
                    let expected = 0L
                    let actual = binarySearch (fun i -> true) -1L 0L
                    Expect.equal actual expected "i = 0"
                }
                test "left over" {
                    let arr = [| 5 .. 10 |]
                    let expected = -1L
                    let actual = binarySearch (fun i -> arr.[int32 i] < 5) (int64 arr.Length) -1L
                    Expect.equal actual expected "i = -1"
                }
                test "right over" {
                    let arr = [| 5 .. 10 |]
                    let expected = int64 arr.Length
                    let actual = binarySearch (fun i -> arr.[int32 i] > 10) -1L (int64 arr.Length)
                    Expect.equal actual expected "i = 5"
                } ]
          testList "leftBinarySearch"
              [ test "left over" {
                    let arr = [| 5 .. 10 |]
                    let expected = None
                    let actual = leftBinarySearch (int64 arr.Length) (fun i -> arr.[int32 i] < 5)
                    Expect.equal actual expected "None"
                } ]
          testList "rightBinarySearch"
              [ test "left over" {
                    let arr = [| 5 .. 10 |]
                    let expected = None
                    let actual = rightBinarySearch (int64 arr.Length) (fun i -> arr.[int32 i] > 10)
                    Expect.equal actual expected "None"
                } ]
          testList "runLengthEncoding"
              [ testOrderedSeqEqual (runLengthEncoding "") [] "empty"
                testOrderedSeqEqual (runLengthEncoding "aabbbbc")
                    [ ("a", 2)
                      ("b", 4)
                      ("c", 1) ] "aabbbbc" ]
          testList "ternarySearchDownward"
              [ testApproximation (ternarySearchDownward 0.0 20.0 (fun x -> (x - 13.0) * (x - 13.0)) 0.000000001) 13.0
                    Accuracy.medium "x = 13" ]
          testList "ternarySearchUpward"
              [ testApproximation (ternarySearchUpward 0.0 20.0 (fun x -> -(x - 13.0) * (x - 13.0)) 0.000000001) 13.0
                    Accuracy.medium "x = 13" ]

          testList "checkFlag"
              [ testCase "ok1" <| fun () ->
                  let expected = true
                  let actual = checkFlag (pown 2 4 + pown 2 1) 4
                  Expect.equal actual expected "10010 4"
                testCase "ok2" <| fun () ->
                    let expected = false
                    let actual = checkFlag (pown 2 4 + pown 2 1) 3
                    Expect.equal actual expected "10010 3"
                test "negative flag" { Expect.throws (fun () -> checkFlag -1 4 |> ignore) "flag < 0" }
                test "negative flagNumber" { Expect.throws (fun () -> checkFlag 1 -1 |> ignore) "flagNumber < 0" } ] ]