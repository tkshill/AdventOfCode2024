module Day02

open Expecto
open Expecto.Flip

let sample = 
    "7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9"

[<Tests>]
let tests =
    testList "Day 02" [
        testCase "Part 1" <| fun _ ->
            Day02.part1 sample
            |> Expect.equal "The expected value is " 2
        testCase "Part 2" <| fun _ ->
            Day02.part2 sample
            |> Expect.equal "The expected value is "  4
    ]