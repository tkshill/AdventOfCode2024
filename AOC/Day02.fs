module Day02

open FParsec
open FParsec.Pipes

let isSafe lst =
    let diff = uncurry (-)
    let safeItem polarityCheck value = 
        polarityCheck (diff value) && abs (diff value) > 0 && abs (diff value) < 4

    forall (safeItem ((>) 0)) lst || forall (safeItem ((<) 0)) lst

let solve safetyCheck =
    let lineParser = %% +.(p<int> * (qty[1..] / ' ')) -- spaces -|> toSeqRS
    
    runLineParser lineParser >> sumBy (pairwise >> toList >> safetyCheck >> toIntB)

let part1 input = solve isSafe input

let isSafe2 (lst: list<int * int>)  =
    seq { 
        yield lst
        yield lst[1..]
        yield lst[0..(length lst - 2)]
        for i in 0..(length lst - 2) do 
        yield lst[0..i - 1] @ [fst lst[i], snd lst[i + 1]] @ lst[i + 2..];            
    } |> exists isSafe 

let part2 input = solve isSafe2 input
