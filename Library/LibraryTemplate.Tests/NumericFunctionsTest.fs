module NumericFunctionsTest

open AtCoder.NumericFunctions
open Expecto
open System
open TestTools

[<Tests>]
let tests =
    let mods = { Mods.divisor = 1000_000_007 }
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
        testList "Mods.Div" [
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
        testList "Mods.FactTable" [
            testError (fun () -> (mods.FactTable 3).[4]) "make too match array"
            testEqual (mods.FactTable 10).[10] 3628800 "10!"
        ]
        testList "CombTable" [
            testError (fun () -> (mods.CombTable 3).[3, 4]) "make too match array"
            testEqual (mods.CombTable 10).[10, 3] 120 "10 C 3"
        ]
    ]