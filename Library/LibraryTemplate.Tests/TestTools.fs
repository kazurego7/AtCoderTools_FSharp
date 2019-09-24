module TestTools

open Expecto

let testEqual (actual : 'a) (expected : 'a) (message : string) : Test =
    test message { Expect.equal actual expected message }
let testError (f : unit -> 'a) (message : string) : Test =
    test message { Expect.throws (f >> ignore) message }
