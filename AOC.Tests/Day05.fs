module Day05

open Expecto
open Expecto.Flip

let sample = 
    "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47"

[<Tests>]
let tests =
    testList "Day 05" [
        testList "Part 1" [   
            testCase "Parser" <| fun _ ->
                Day05.part1 sample
                |> Expect.equal "The expected value is " 143;           
            // testCase "final check" <| fun _ ->
            //     Day05.parsesample
            //     |> Expect.equal "The expected value is " 18;
            ]
        testCase "Part 2" <| fun _ ->
            Day05.part2 sample
            |> Expect.equal "The expected value is "  123
    ]