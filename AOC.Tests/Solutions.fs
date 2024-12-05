module Tests

open Expecto

[<Tests>]
let tests =
    testList "All days" [
        Day01.tests;
        Day02.tests;
        Day03.tests;
    ]