module Day11

open FParsec
open FParsec.Pipes

let solomon = string >> splitInto 2 >> map (System.String >> uint64) >> toList

let blinkStone = function
    | 0UL -> [1UL]
    | x when string x |> length |> isEven -> solomon x
    | x -> [x * 2024UL]

let updates (stone, occurances) = blinkStone stone |> map (fun k -> k, occurances)

let rec folder stoneOccurences  = function
    | 0 -> sumBy snd stoneOccurences
    | blinksLeft ->
        collect updates stoneOccurences 
        |> groupBy fst
        |> map (mapT id (sumBy snd))
        |> flip folder (blinksLeft - 1)

let solve totalBlinks  =
    runParser (sepBy1 puint64 %' ')
    >> countBy id
    >> map (mapT id uint64)
    >> flip folder totalBlinks

let part1 input = solve 25 input

let part2 input = solve 75 input