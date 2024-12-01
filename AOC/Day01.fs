module Day01

open FParsec
open FParsec.Pipes

let parse input = 
    split "\n" input
    |> map (runParser (%% +.pint32 -- spaces -- +.pint32-%> auto))
    |> fun lsts -> map fst lsts, map snd lsts

let part1 input =
    parse input
    |> mapT sort sort
    |> uncurry zip
    |> map (uncurry (-) >> abs)
    |> sum

let calcScore groups n  =
    tryFind (fst >> (=) n) groups
    |> Option.map (snd >> length >> ((*) n))
    |> Option.defaultValue 0

let part2 input =
    parse input
    |> mapT id (groupBy id)
    |> flipT
    |> foldT calcScore map
    |> sum