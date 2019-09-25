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
            testSeqEqual (runLengthEncoding "") [] "empty"
            testSeqEqual (runLengthEncoding "aabbbbc") [ ("a", 2); ("b", 4); ("c", 1) ] "aabbbbc"
        ]
        testList "ternarySearchDownward" [
            testApproximation (ternarySearchDownward 0.0 20.0 (fun x -> (x - 13.0) * (x - 13.0)) 0.000000001) 13.0 Accuracy.medium "x = 13"
        ]
        testList "ternarySearchUpward" [
            testApproximation (ternarySearchUpward 0.0 20.0 (fun x -> -(x - 13.0) * (x - 13.0)) 0.000000001) 13.0 Accuracy.medium "x = 13"
        ]
    ]
// testList "twoPointers" [
//     let arr1 = [| 5; 1; 2; 5; 10; 7; 4; 9; 2; 8 |]
//     let sumLR = fun l r -> Array.take r arr1 |> Array.skip l |> Array.sum
//     testEqual (twoPointers 10 (fun l r -> sumLR l r >= 12) 0 (fun l r cond -> cond + 1) (fun l r cond -> cond)) 11 "counting"
//     testEqual (twoPointers 10 (fun l r -> sumLR l r >= 28) 600 (fun l r cond -> cond) (fun l r cond -> min (r - l) cond)) 11 "min interval"
// ]
