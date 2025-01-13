module Day13

open FParsec
open FParsec.Pipes

let inputParser = 
    let lineParser = %["Button A: "; "Button B: "; "Prize: " ] >>. %["X+"; "X="] >>. %p<double> .>> %[", Y+"; ", Y="] .>>. %p<double>

    sepEndBy1 lineParser (many1 newline)

let toSimultaneous offset ((x1, y1) :: (x2, y2) :: (prizeX, prizeY) :: _) = (x1, x2, prizeX + offset), (y1, y2, prizeY + offset)

let solveSimulatenous ((a1, b1, c1), (a2, b2, c2)) = 
    let Bpresses = (a1 * c2 - a2 * c1) / (a1 * b2 - a2 * b1)
    let Apresses = (c1 - b1 * Bpresses) / a1

    if Apresses = floor Apresses && Bpresses = floor Bpresses then Apresses * 3.0 + Bpresses else 0.0

let solve offset = runParser inputParser >> chunkBySize 3 >> sumBy (toList >> toSimultaneous offset >> solveSimulatenous)

let part1 input = solve 0 input

let part2 input = solve 10000000000000.0 input