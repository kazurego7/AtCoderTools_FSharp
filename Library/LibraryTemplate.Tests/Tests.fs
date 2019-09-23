module Tests

open AtCoder
open Expecto
open System

let testEqual (actual : 'a) (expected : 'a) (message : string) : Test =
    test message { Expect.equal actual expected message }
let testError (f : unit -> 'a) (message : string) : Test =
    test message { Expect.throws (f >> ignore) message }

[<Tests>]
let tests =
    let mods = { NumericFunctions.Mods.divisor = 1000_000_007 }
    testList "NumericFunctions" [
        testList "Mods.Mod" [
            testEqual (mods.Mod(int64 Int32.MaxValue + 1L)) 147483634 "over Int32"
            testEqual (mods.Mod -5) 1000000002 "minus"
        ]
        testList "Mods.Add" [
            testEqual (mods.Add Int32.MaxValue 1) 147483634 "over Int32"
        ]
        testList "Mods.Sub" [
            testEqual (mods.Sub Int32.MinValue 1) 852516372 "under Int32"
        ]
        testList "Mods.Mul" [
            testEqual (mods.Mul (1 <<< 17) (1 <<< 17)) 179869065 "over Int32"
        ]
        testList "Mods.Pow" [
            testEqual (mods.Pow 17 11) 896067736 "n <> 0 && b <> 0 && over Int32"
            testEqual (mods.Pow 0 0) 1 "n = 0 && b = 0"
            testEqual (mods.Pow 0 41) 0 "n = 0"
            testEqual (mods.Pow 151 0) 1 "b = 0"
            testEqual (mods.Pow 1 1000) 1 "StackOverFlowかもしれない"
            ]
        // testList "Mods.Inv" [
        //     testEqual (mods.Inv 100000)
        // ]
        ptestList "Mods.Div" [
            testEqual (mods.Div 678813585 100000) 123456789 "div"
        ]
        testList "Mods.Perm" [
            testError (fun () -> mods.Perm -7 31) "n < 0"
            testError (fun () -> mods.Perm 13 -7) "k < 0"
            testEqual (mods.Perm 23 7) 235591273 "n <> 0 && k <> 0 && over Int32"
            testEqual (mods.Perm 0 0) 1 "n = 0 && k = 0"
            testEqual (mods.Perm 0 516) 0 "n = 0"
            testEqual (mods.Perm 5 0) 1 "k = 0"
            testEqual (mods.Perm 4 23) 0 "n < k"
            ]
    ]
