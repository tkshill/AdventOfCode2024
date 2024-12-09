module Day08

open Expecto
open Expecto.Flip

let sample = 
    "............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............"

[<Tests>]
let tests =
    testList "Day 08" [
        testCase "Part 1" <| fun _ ->
            Day08.part1 sample
            |> Expect.equal "The expected value is " 14;           
        testCase "Part 2" <| fun _ ->
            Day08.part2 sample
            |> Expect.equal "The expected value is " 34;
    ]