module Day07.fs

open FParsec
open FParsec.Pipes

let pLine = %% +.pint32 -- ' ' -- +.(sepEndBy1 pint32 %' ') -%> auto

let solve (goal, values) =
    seq { for x in values do for y in values do if x + y = goal then yield x * y }

    0
let part1 input =
    split "\n" input |> map (runParser pLine) 
    |> sumBy solve