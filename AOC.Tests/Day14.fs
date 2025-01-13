module Day14

open Expecto
open Expecto.Flip
 
let sample = 
    "p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3"

[<Tests>]
let tests =
    testList "Day 14" [
        testCase "Part 1" <| fun _ ->
            Day14.part1 sample
            |> Expect.equal "The expected value is " 12;
    ]