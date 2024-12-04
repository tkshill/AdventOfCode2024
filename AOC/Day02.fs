module Day02

open FSharpx.Collections
open FParsec
open FParsec.Pipes

let isSafe lst =
    let diff = uncurry (-)
    let safeItem f value = f (diff value) && abs (diff value) > 0 && abs (diff value) < 4

    forall (safeItem ((>) 0)) lst || forall (safeItem ((<) 0)) lst

let solve safeCheck =
    let lineParser = %% +.(p<int> * (qty[1..] / ' ')) -- spaces -|> ResizeArray.toSeq
    let counter = 
        runParser lineParser 
        >> pairwise 
        >> toList 
        >> safeCheck 
        >> function | true -> 1 | _ -> 0

    split "\n" >> sumBy counter

let part1 input =
    solve isSafe input

let isSafe2 (lst: list<int * int>)  =
    seq { 
        yield lst
        yield lst[1..]
        yield lst[0..(length lst - 2)]
        for i in 0..(length lst - 2) do
            yield lst[0..i - 1] @ [fst lst[i], snd lst[i + 1]] @ lst[i + 2..];            
    } |> exists isSafe 

let part2 input =
    solve isSafe2 input
