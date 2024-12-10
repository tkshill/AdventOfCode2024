module Day03

open FParsec
open FParsec.Pipes

let parseText = 
    let pMul = %% "mul(" -- +.pint32 -- ',' -- +.pint32 -? ')' -|> curry Some

    %% +.(%[pMul; (anyChar >>% None)] * qty[0..]) -%> auto

let part1 input =
    runParser parseText input |> (choose id >> sumBy (uncurry mul))

let pSet boolean = 
    (if boolean then %"do()" else %"don't()") .>> setUserState boolean >>% None

let pMul2 =
    let pMul = %% "mul(" -- +.(pint32) -- ',' -- +.(pint32) -? ')'  -|> curry Some

    pipe2 pMul getUserState 
    <| curry (function | result, true -> result | _ -> None)

let parseText2 = 
    %% +.(%[pMul2; pSet false; pSet true; anyChar >>% None] * (qty[0..] )) -%> auto

let part2 input = 
    runParserWithState parseText2 true  input 
    |> (choose id >> sumBy (uncurry mul))