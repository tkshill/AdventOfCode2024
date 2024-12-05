module Day03

open FParsec
open FParsec.Pipes
open FSharpx.Collections
open System

let solve = ResizeArray.toSeq >> choose id >> sumBy (uncurry (*))

let parseText = 
    let pUpto3 = %% +.(digit * qty[1..3]) -|> (ResizeArray.toArray >> String >> int)
    let pMultiplier = %% "mul(" -- +.(pUpto3) -- ',' -- +.(pUpto3) -- ')'  -|> curry Some
    let pNotM = manySatisfy ((<>) 'm')
    let pM = %'m' >>% None

    %% pNotM -- +.([attempt pMultiplier; pM] * (qty[1..] /. pNotM)) -%> auto

let part1 input = runParser parseText input |> solve

let pSet boolean = 
    let parser = if boolean then pstring "do()" else pstring "don't()"

    parser .>> setUserState boolean >>% None

let pMultiplier2 =
    let pUpto3 = %% +.(digit * qty[1..3]) -|> (ResizeArray.toArray >> String >> int)
    let pMultiplier = %% "mul(" -- +.(pUpto3) -- ',' -- +.(pUpto3) -- ')'  -|> curry Some

    pipe2 pMultiplier getUserState <| curry (function | result, true -> result | _ -> None)

let parseText2 =
    let pD = %'d' >>% None
    let pM = %'m' >>% None
    let pNotMD = manySatisfy (fun c -> c <> 'm' && c <> 'd')

    %% pNotMD -- +.([attempt pMultiplier2; attempt (pSet false); attempt (pSet true); attempt pM; pD;] * (qty[1..] /. pNotMD)) -%> auto

let part2 input = runParserWithState parseText2 true  input |> solve