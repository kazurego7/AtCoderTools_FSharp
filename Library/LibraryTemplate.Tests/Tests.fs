module Tests

open Expecto
open AtCoder

let testEq (actual : 'a) (expected : 'a) (message : string) : Test =
    test message { Expect.equal actual expected message }
let mods = { NumericFunctions.Mod.divisor = 1_000_000_007 }
let powTest =
    testList "Pow" [ testEq (mods.Pow 0L 41) 0 "0^n = 0 (ただし n != 0)"
                     testEq (mods.Pow 151L 0) 1 "b^0 = 1"
                     testEq (mods.Pow 0L 0) 1 "0^0 = 0"
                     testEq (mods.Pow 1L 1000) 1 "StackOverFlowかもしれない" ]
let permTest =
    testList "Perm" [ testEq (mods.Perm 5 0) 1 "k = 0"
                      testEq (mods.Perm 4 23) 0 "n < k"
                      testEq (mods.Perm 13 -7) 0 "k < 0"
                      testEq (mods.Perm 0 0) 1 "n = k = 0" ]

[<Tests>]
let tests = testList "NumericFunctions" [ powTest; permTest ]
