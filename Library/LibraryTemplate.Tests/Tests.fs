module Tests

open Expecto
open AtCoder

let testEq (actual : 'a) (expected : 'a) (message : string) : Test =
    test message { Expect.equal actual expected message }
let testError (f : unit -> 'a) (message : string) : Test =
    test message { Expect.throws (fun () -> f() |> ignore) message }
let mods = { NumericFunctions.Mod.divisor = 1_000_000_007 }
let powTest =
    testList "Pow" [ testEq (mods.Pow 17L 11) 896067736
                         "n <> 0 && b <> 0 && over Int32.MaxValue"
                     testEq (mods.Pow 0L 0) 1 "n = 0 && b = 0"
                     testEq (mods.Pow 0L 41) 0 "n = 0"
                     testEq (mods.Pow 151L 0) 1 "b = 0"
                     testEq (mods.Pow 1L 1000) 1 "StackOverFlowかもしれない" ]
let permTest =
    testList "Perm" [ testEq (mods.Perm 23 7) 235591273
                          "n <> 0 && k <> 0 && over Int32.MaxValue"
                      testEq (mods.Perm 0 0) 1 "n = 0 && k = 0"
                      testEq (mods.Perm 0 516) 0 "n = 0"
                      testEq (mods.Perm 5 0) 1 "k = 0"
                      testEq (mods.Perm 4 23) 0 "n < k"
                      testError (fun () -> mods.Perm -7 31) "n < 0"
                      testError (fun () -> mods.Perm 13 -7) "k < 0" ]

[<Tests>]
let tests = testList "NumericFunctions" [ powTest; permTest ]
