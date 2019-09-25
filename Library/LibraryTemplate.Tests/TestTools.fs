module TestTools

open Expecto

let testValueEqual (actual : 'a) (expected : 'a) (message : string) : Test =
    test message { Expect.equal actual expected message }
let testSeqEqual (actual : seq<'a>) (expected : seq<'a>) (message : string) : Test =
    test message { Expect.containsAll actual expected message }
let testApproximation (actual : float) (expected : float) (accuracy : Accuracy) (message : string) : Test =
    test message { Expect.floatClose accuracy actual expected message }
let testError (f : unit -> 'a) (message : string) : Test =
    test message { Expect.throws (f >> ignore) message }
