module Day03

open FParsec
open FParsec.Pipes
open FSharpx.Collections
open System

let parseText = 
    let pUpto3 = %% +.(digit * qty[1..3]) -|> (ResizeArray.toArray >> String >> int)
    let pMultiplier = %% "mul(" -- +.(pUpto3) -- ',' -- +.(pUpto3) -- ')'  -|> curry Some
    let pNotM = manySatisfy ((<>) 'm')
    let pM = %% 'm' -|> None

    %% pNotM -- +.([attempt pMultiplier; pM] * (qty[1..] /. pNotM)) -|> 
        (ResizeArray.toSeq >> choose id >> sumBy (uncurry (*)))

let part1 input =
    runParser parseText input 

let pSet boolean = 
    parse {
        do! (%% (if boolean then "do()" else "don't()") -|> ())
        do! setUserState boolean

        return None
    }

let pMultiplier2 =
    let pUpto3 = %% +.(digit * qty[1..3]) -|> (ResizeArray.toArray >> String >> int)
    let pMultiplier = %% "mul(" -- +.(pUpto3) -- ',' -- +.(pUpto3) -- ')'  -|> curry Some

    parse {
        let! mulsR = pMultiplier
        let! canMul = getUserState

        if canMul then return mulsR else return None
    } 

let parseText2 =
    let pD = %% 'd' -|> None
    let pM = %% 'm' -|> None
    let pNotMD = manySatisfy (fun c -> c <> 'm' && c <> 'd')

    %% pNotMD -- +.([attempt pMultiplier2; attempt (pSet false); attempt (pSet true); attempt pM; pD;] * (qty[1..] /. pNotMD)) -|> 
        (ResizeArray.toSeq >> choose id >> sumBy (uncurry (*)))

let part2 input = 
    runParserOnString parseText2 true "" input
    |> function 
        | Success(x, _, _) -> x 
        | Failure(x, _, _) -> failwith $"Parser Error: {x}"
