module Day01

open Expecto
open Expecto.Flip

let sample = 
    "3   4
4   3
2   5
1   3
3   9
3   3"

[<Tests>]
let tests =
    ptestList "Day 01" [
        testCase "Part 1" <| fun _ ->
            Day01.part1 sample
            |> Expect.equal "The expected value is " 11
        testCase "Part 2" <| fun _ ->
            Day01.part2 sample
            |> Expect.equal "The expected value is "  31
    ]