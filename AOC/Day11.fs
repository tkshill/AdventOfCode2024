module Day11

open FParsec
open FParsec.Pipes
open System

let parse = sepBy1 puint64 %' '

let solomon  = string >> splitInto 2 >> map (String >> uint64) >> toList

let updateNum = function
    | 0UL -> [1UL]
    | x when string x |> length |> isEven -> solomon x
    | x -> [x * 2024UL]

let updates (stone, count) = 
    updateNum stone |> map (fun k -> k, count)

let folder  =
    collect updates 
    >> groupBy fst
    >> map (mapT id (sumBy snd)) 
    >> logF (sumBy snd)

let solve limit  =
    runParser parse  
    >> Seq.countBy id
    >> map (mapT id uint64)
    >> iterate folder
    >> take (limit + 1)
    >> last
    >> sumBy snd

let part1 input = solve 25 input

let part2 input = solve 75 input