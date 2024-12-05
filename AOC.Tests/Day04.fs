module Day04

open Expecto
open Expecto.Flip

let sample = 
    "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX"

[<Tests>]
let tests =
    testList "Day 04" [
        testList "Part 1" [              
            testCase "final check" <| fun _ ->
                Day04.part1 sample
                |> Expect.equal "The expected value is " 18;
            ]
        testCase "Part 2" <| fun _ ->
            Day04.part2 sample
            |> Expect.equal "The expected value is "  9
    ]