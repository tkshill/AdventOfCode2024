module Day09

open Expecto
open Expecto.Flip

let sample = 
    "2333133121414131402"

[<Tests>]
let tests =
    ptestList "Day 09" [
        testCase "Part 1" <| fun _ ->
            Day09.part1 sample
            |> Expect.equal "The expected value is " 1928;
        testCase "Part 2" <| fun _ ->
            Day09.part2 sample
            |> Expect.equal "The expected value is " 2858;
    ]