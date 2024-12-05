module Day03

open Expecto
open Expecto.Flip

let sample = 
    "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

let sample2 =
    "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

[<Tests>]
let tests =
    testList "Day 03" [
        testList "Part 1" [              
            testCase "final check" <| fun _ ->
                Day03.part1 sample
                |> Expect.equal "The expected value is " 161;
            ]
        testCase "Part 2" <| fun _ ->
            Day03.part2 sample2
            |> Expect.equal "The expected value is "  48
    ]