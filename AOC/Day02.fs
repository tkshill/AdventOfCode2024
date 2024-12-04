module Day02

open FSharpx.Collections
open FParsec
open FParsec.Pipes

let lineParser = %% +.(p<int> * (qty[1..] / ' ')) -- spaces -|> ResizeArray.toSeq

let isSafe lst =
    let diff = uncurry (-)
    let safeItem f value = f (diff value) && abs (diff value) > 0 && abs (diff value) < 4
  
    [(>) 0; (<) 0]
    |> map (safeItem >> flip forall lst)
    |> reduce (||)

let solve safeCheck =
    split "\n"
    >> filter (runParser lineParser >> pairwise >> toList >> safeCheck)
    >> length

let part1 input =
    solve isSafe input

let isSafe2 (lst: list<int * int>)  =
        seq { 
            for i in -1..(length lst - 1) do
                yield lst 
                if i = -1 then yield lst[1..]
                else if i = length lst - 1 then yield lst[0..(length lst - 2)]
                else yield lst[0..i - 1] @ [fst lst[i], snd lst[i + 1]] @ lst[i + 2..]            
        } |> exists isSafe 

let part2 input =
    solve isSafe2 input
