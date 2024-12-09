module Day01

open FParsec
open FParsec.Pipes

let parse  = 
    split "\n" >> map (runParser (%% +.p<int> -- spaces -- +.p<int> -%> auto)) >> fun lsts -> map fst lsts, map snd lsts

let part1 input = parse input |> spread sort |> uncurry zip |> map (uncurry (-) >> abs) |> sum

let calcScore distincts n  = tryFind (fst >> (=) n) distincts |> mapO (snd >> ((*) n)) |> withDefault 0

let part2 input = parse input |> mapSnd countDistinct |> flipT |> foldT calcScore map |> sum
