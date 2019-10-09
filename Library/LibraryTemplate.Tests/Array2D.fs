module AtCoder.Array2D.Test

open AtCoder
open Expecto
open System

[<Tests>]
let tests =
    testList "Array2D"
        [ testList "transpose"
              [ testCase "empty" <| fun () -> Expect.equal [| [||] |] [| [||] |] "empty -> empty"
                testCase "rowVector"
                    (fun () ->
                    let expected =
                        [ [ 1 ]
                          [ 2 ]
                          [ 3 ] ]
                        |> array2D

                    let actual =
                        [ [ 1 .. 3 ] ]
                        |> array2D
                        |> Array2D.transpose

                    Seq.interval 0 3
                    |> Seq.iter
                        (fun i ->
                        Expect.sequenceContainsOrder expected.[i, *] actual.[i, *] "[ [1; 2; 3] ] -> [ [1]; [2]; [3] ]"))
                testCase "columnVector"
                    (fun () ->
                    let expected = [ [ 1 .. 3 ] ] |> array2D

                    let actual =
                        [ [ 1 ]
                          [ 2 ]
                          [ 3 ] ]
                        |> array2D
                        |> Array2D.transpose

                    Seq.interval 0 3
                    |> Seq.iter
                        (fun i ->
                        Expect.sequenceContainsOrder expected.[*, i] actual.[*, i] "[ [1]; [2]; [3] ] -> [ [1; 2; 3] ]"))
                testCase "matrix"
                    (fun () ->
                    let expected =
                        [ [ 1; 4 ]
                          [ 2; 5 ]
                          [ 3; 6 ] ]
                        |> array2D

                    let actual =
                        [ [ 1; 2; 3 ]
                          [ 4; 5; 6 ] ]
                        |> array2D
                        |> Array2D.transpose

                    for i in Seq.interval 0 3 do
                        for k in Seq.interval 0 2 do
                            Expect.equal expected.[i, k] actual.[i, k]
                                "[ [1;4]; [2;5]; [3;6] ] -> [ [1; 2; 3]; [4; 5; 6] ]") ] ]
