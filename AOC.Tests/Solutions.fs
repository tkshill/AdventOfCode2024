module Tests

open Expecto

[<Tests>]
let tests =
    testList "All days" [
        Day01.tests;
        Day02.tests;
        Day03.tests;
        Day04.tests;
        Day05.tests;
        Day06.tests;
        Day07.tests;
        Day08.tests;
        Day09.tests;
        Day10.tests;
        Day11.tests;
        Day12.tests;
        Day13.tests;
        Day14.tests;
    ]