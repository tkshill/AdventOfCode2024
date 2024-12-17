module Day12

open Expecto
open Expecto.Flip

let sample = 
    "RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE"

[<Tests>]
let tests =
    testList "Day 12" [
        testCase "Part 1" <| fun _ ->
            Day12.part1 sample
            |> Expect.equal "The expected value is " 1930;
        // testCase "Part 2" <| fun _ ->
        //     Day12.part2 sample
        //     |> Expect.equal "The expected value is " 1206;

    ]