module Day07

open FParsec
open FParsec.Pipes 

let parseLine = (pint64 |>> bigint) .>> %": " .>>. sepEndBy1 (pint64 |>> bigint) %' '

let rec options ops n = 
    let subPerms perm = map (fun op -> seq { yield op; yield! perm }) ops

    seq { if n = 0 then yield empty else for perm in options ops (n - 1) do yield! subPerms perm } 
    
let hasMatch ops (goal, head :: rest) = 
    options ops (length rest) |> exists (map2 (flip apply) rest >> foldl (flip apply) head >> eq goal)

let part1 input =
    split "\n" input |> map (runParser parseLine) |> filter (hasMatch [(*); (+)]) |> sumBy fst

let combine i j = $"{j}{i}" |> int64 |> bigint

let part2 input = 
    split "\n" input |> map (runParser parseLine) |> filter (hasMatch [(*); (+); combine]) |> sumBy fst 
