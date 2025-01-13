module Day13

open FParsec
open FParsec.Pipes

let pLine = 
    %["Button A: "; "Button B: "; "Prize: " ] >>. %["X+"; "X="] >>. %p<double> .>> %[", Y+"; ", Y="] .>>. %p<double>

let pInput = sepEndBy1 pLine (many1 newline)

let toEquations offset ((x1, y1) :: (x2, y2) :: (prizeX, prizeY) :: _) = 
    (x1, x2, prizeX + offset), (y1, y2, prizeY + offset)

let solveSimulatenous ((a1, b1, c1), (a2, b2, c2)) = 
    let y = (a1 * c2 - a2 * c1) / (a1 * b2 - a2 * b1)
    let x = (c1 - b1 * y) / a1

    if x = floor x && y = floor y then x * 3.0 + y else 0.0

let solve offset = 
    runParser pInput >> chunkBySize 3 >> sumBy (toList >> toEquations offset >> solveSimulatenous)

let part1 input = solve 0 input

let part2 input = solve 10000000000000.0 input