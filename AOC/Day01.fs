module Day01

open FParsec
open FParsec.Pipes

let parse  = 
    split "\n" 
    >> map (runParser (%% +.pint32 -- spaces -- +.pint32-%> auto))
    >> fun lsts -> map fst lsts, map snd lsts

let part1 input =
    parse input
    |> mapT sort sort
    |> uncurry zip
    |> map (uncurry (-) >> abs)
    |> sum

let calcScore groups n  =
    tryFind (fst >> (=) n) groups
    |> mapO (snd >> ((*) n))
    |> withDefault 0

let part2 input =
    parse input
    |> mapSnd group
    |> flipT
    |> foldT calcScore map
    |> sum
