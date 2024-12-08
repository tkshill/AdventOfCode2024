module Day06

open Expecto
open Expecto.Flip

let sample = 
    "....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...
"

[<Tests>]
let tests =
    ptestList "Day 06" [
        testCase "Parser" <| fun _ ->
            Day06.part1 sample
            |> Expect.equal "The expected value is " 41;           

        testCase "Part 2" <| fun _ ->
            Day06.part2 sample
            |> Expect.equal "The expected value is "  6;
    ]