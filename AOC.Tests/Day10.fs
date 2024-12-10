module Day10

open Expecto
open Expecto.Flip

let sample = 
    "89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732"

[<Tests>]
let tests =
    testList "Day 10" [
        testCase "Part 1" <| fun _ ->
            Day10.part1 sample
            |> Expect.equal "The expected value is " 36;
        testCase "Part 2" <| fun _ ->
            Day10.part2 sample
            |> Expect.equal "The expected value is " 81;
    ]