module Algorithm

open AtCoder.Algorithm
open Expecto
open System
open TestTools

[<Tests>]
let tests =
    testList "Algorithm"
        [ testList "binarySearch"
              [ testEqual (binarySearch [||] (fun _ -> true) -1 0) 0 "empty"

                testEqual
                    (binarySearch [| 5..10 |] (fun x -> x > 10) -1 [| 5..10 |].Length)
                    [| 5..10 |].Length "right over"

                testEqual
                    (binarySearch [| 5..10 |] (fun x -> x < 5) [| 5..10 |].Length -1) -1
                    "left over" ]

          testList "leftBinarySearch"
              [ testEqual (leftBinarySearch [| 5..10 |] (fun x -> x < 5)) None "left over" ]

          testList "rightBinarySearch"
              [ testEqual (rightBinarySearch [| 5..10 |] (fun x -> x > 10)) None
                    "right over" ] ]
// testList "twoPointers" [
//     let arr1 = [| 5; 1; 2; 5; 10; 7; 4; 9; 2; 8 |]
//     let sumLR = fun l r -> Array.take r arr1 |> Array.skip l |> Array.sum
//     testEqual (twoPointers 10 (fun l r -> sumLR l r >= 12) 0 (fun l r cond -> cond + 1) (fun l r cond -> cond)) 11 "counting"
//     testEqual (twoPointers 10 (fun l r -> sumLR l r >= 28) 600 (fun l r cond -> cond) (fun l r cond -> min (r - l) cond)) 11 "min interval"
// ]
