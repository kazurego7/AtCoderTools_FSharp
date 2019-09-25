module Algorithm

open AtCoder.Algorithm
open Expecto
open System
open TestTools

[<Tests>]
let tests =
    testList "Algorithm" [
        testList "binarySearch" [
            testEqual (binarySearch [||] (fun _ -> true) -1 0) 0 "empty"
            testEqual (binarySearch [| 5..10 |] (fun x -> x > 10) -1 [| 5..10 |].Length) [| 5..10 |].Length "right over"
            testEqual (binarySearch [| 5..10 |] (fun x -> x < 5) [| 5..10 |].Length -1) -1 "left over"
        ]
        testList "leftBinarySearch" [
            testEqual (leftBinarySearch [| 5..10 |] (fun x -> x < 5)) None "left over"
        ]
        testList "rightBinarySearch" [
            testEqual (rightBinarySearch [| 5..10 |] (fun x -> x > 10)) None "right over"
        ]
    ]
