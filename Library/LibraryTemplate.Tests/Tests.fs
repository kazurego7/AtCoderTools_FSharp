module Tests

open Expecto
open AtCoder

let testEq (actual : 'a) (expected : 'a) (message : string) : Test =
    test message { Expect.equal actual expected message }
let testThrow (f : unit -> 'a) (message : string) : Test =
    test message { Expect.throws (fun () -> f() |> ignore) message }
let mods = { NumericFunctions.Mod.divisor = 1_000_000_007 }
let powTest =
    testList "Pow" [ testEq (mods.Pow 13L 7) 62748517 "n <> 0 && k <> 0"
                     testEq (mods.Pow 0L 0) 1 "n = 0 && k = 0"
                     testEq (mods.Pow 0L 41) 0 "n = 0"
                     testEq (mods.Pow 151L 0) 1 "k = 0"
                     testEq (mods.Pow 1L 1000) 1 "StackOverFlowかもしれない" ]
let permTest =
    testList "Perm" [ testEq (mods.Perm 13 7) 8648640 "n <> 0 && k <> 0"
                      testEq (mods.Perm 0 0) 1 "n = 0 && k = 0"
                      testEq (mods.Perm 0 516) 0 "n = 0"
                      testEq (mods.Perm 5 0) 1 "k = 0"
                      testEq (mods.Perm 4 23) 0 "n < k"
                      testThrow (fun () -> mods.Perm -7 31) "n < 0"
                      testThrow (fun () -> mods.Perm 13 -7) "k < 0" ]

[<Tests>]
let tests = testList "NumericFunctions" [ powTest; permTest ]
