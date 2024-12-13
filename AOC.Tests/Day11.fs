module Day11

open Expecto
open Expecto.Flip

let sample = 
    "125 17"

[<Tests>]
let tests =
    ptestList "Day 11" [
        testCase "Part 1" <| fun _ ->
            Day11.part1 sample
            |> Expect.equal "The expected value is " 55312UL;

    ]