module Day14

open FParsec
open FParsec.Pipes

type Quadrants = { Q1 : int; Q2 : int; Q3 : int; Q4 : int }

let pLine: Parser<((int * int) * (int * int)), unit> = 
    %% %"p=" -- +. pint32 -- %',' -- +. pint32 -- %" v=" -- +. pint32 -- %',' -- +. pint32  -|> 
        fun px py vx vy -> (px, py), (vx, vy)

let transform xLimit yLimit ticks ((px, py), (vx, vy)) =
    (abs (vx * ticks) + px) % xLimit, (abs (vy * ticks) + py) % yLimit

let collapse qs = function
    | (x, y) when x < 6  && y < 4 -> { qs with Q1 = qs.Q1 + 1 }
    | (x, y) when x > 6  && y < 4 -> { qs with Q2 = qs.Q2 + 1 }
    | (x, y) when x < 6  && y > 4 -> { qs with Q3 = qs.Q3 + 1 }
    | (x, y) when x > 6  && y > 4 -> { qs with Q4 = qs.Q4 + 1 }
    | _ -> qs 

let part1 input =
    input
    |> split "\n"
    |> map (runParser pLine >> transform 11 7 100 >> log)
    |> foldl collapse { Q1 = 0; Q2 = 0; Q3 = 0; Q4 = 0 } |> log
    |> fun qs -> qs.Q1 + qs.Q2 + qs.Q3 + qs.Q4