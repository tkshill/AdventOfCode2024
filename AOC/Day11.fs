module Day11

open FParsec
open FParsec.Pipes
open System

let solomon = 
    string >> splitInto 2 >> map (String >> uint64) >> toList

let updateNum = function
    | 0UL -> [1UL]
    | x when string x |> length |> isEven -> solomon x
    | x -> [x * 2024UL]

let updates (stone, count) = 
    updateNum stone |> map (fun k -> k, count)

let rec folder input  = function
    | 0 -> sumBy snd input
    | n ->
        collect updates input 
        |> groupBy fst
        |> map (mapT id (sumBy snd))
        |> flip folder (n - 1)

let solve limit  =
    runParser (sepBy1 puint64 %' ')
    >> Seq.countBy id
    >> map (mapT id uint64)
    >> flip folder limit

let part1 input = solve 25 input

let part2 input = solve 75 input