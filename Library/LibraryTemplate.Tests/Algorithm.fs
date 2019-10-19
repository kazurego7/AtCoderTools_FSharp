module AtCoder.Algorithm.Test

open AtCoder.Algorithm
open Expecto
open System
open TestTools

[<Tests>]
let tests =
    testList "Algorithm"
        [ testList "binarySearch"
              [ testValueEqual (binarySearch [||] (fun _ -> true) -1 0) 0 "empty"
                testValueEqual (binarySearch [| 5 .. 10 |] (fun x -> x > 10) -1 [| 5 .. 10 |].Length)
                    [| 5 .. 10 |].Length "right over"
                testValueEqual (binarySearch [| 5 .. 10 |] (fun x -> x < 5) [| 5 .. 10 |].Length -1) -1 "left over" ]
          testList "leftBinarySearch"
              [ testValueEqual (leftBinarySearch [| 5 .. 10 |] (fun x -> x < 5)) None "left over" ]
          testList "rightBinarySearch"
              [ testValueEqual (rightBinarySearch [| 5 .. 10 |] (fun x -> x > 10)) None "right over" ]
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