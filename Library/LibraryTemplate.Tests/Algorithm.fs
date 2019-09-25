module Algorithm

open AtCoder.Algorithm
open Expecto
open System
open TestTools

[<Tests>]
let tests =
    testList "Algorithm" [
        testList "binarySearch" [
            testValueEqual (binarySearch [||] (fun _ -> true) -1 0) 0 "empty"
            testValueEqual (binarySearch [| 5..10 |] (fun x -> x > 10) -1 [| 5..10 |].Length) [| 5..10 |].Length "right over"
            testValueEqual (binarySearch [| 5..10 |] (fun x -> x < 5) [| 5..10 |].Length -1) -1 "left over"
        ]
        testList "leftBinarySearch" [
            testValueEqual (leftBinarySearch [| 5..10 |] (fun x -> x < 5)) None "left over"
        ]
        testList "rightBinarySearch" [
            testValueEqual (rightBinarySearch [| 5..10 |] (fun x -> x > 10)) None "right over"
        ]
        testList "runLengthEncoding" [
            testOrderedSeqEqual (runLengthEncoding "") [] "empty"
            testOrderedSeqEqual (runLengthEncoding "aabbbbc") [ ("a", 2); ("b", 4); ("c", 1) ] "aabbbbc"
        ]
        testList "ternarySearchDownward" [
            testApproximation (ternarySearchDownward 0.0 20.0 (fun x -> (x - 13.0) * (x - 13.0)) 0.000000001) 13.0 Accuracy.medium "x = 13"
        ]
        testList "ternarySearchUpward" [
            testApproximation (ternarySearchUpward 0.0 20.0 (fun x -> -(x - 13.0) * (x - 13.0)) 0.000000001) 13.0 Accuracy.medium "x = 13"
        ]
    ]
