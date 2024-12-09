module Day07

open Expecto
open Expecto.Flip

let sample = 
    "190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20"

[<Tests>]
let tests =
    ptestList "Day 07" [
        ptestCase "Can permute" <| fun _ ->
            Day07.permutations [(*); (+)] 2
            |> length
            |> Expect.equal "The expected value is " 4;
        ptestCase "Part 1" <| fun _ ->
            Day07.part1 sample
            |> Expect.equal "The expected value is " <| bigint 3749;           
        testCase "Part 2" <| fun _ ->
            Day07.part2 sample
            |> Expect.equal "The expected value is "  <| bigint 11387;
    ]