module Day04

let addToMap charNums (dict: Map<int * int, int>) (key, letter) = 

    tryFind (fst >> eq letter) charNums
    |> mapO (snd >> curry dict.Add key)
    |> withDefault dict

let isXmas (dict: Map<int * int, int>) key transform =
    let isXmas_  = function
        | k, Some v when dict.TryFind k = Some (v + 1) -> Some ((), (transform k, dict.TryFind k))
        | _ -> None
    
    unfold isXmas_ (transform key, dict.TryFind key) |> truncate 3 |> length |> eq 3

let xmasCount dict key =

    [ 1, 1; -1, -1; -1, 1; 1, -1; 0, 1; 0, -1; 1, 0; -1, 0 ]
    |> map (spread inc >> uncurry mapT)
    |> sumBy (isXmas dict key >> intB)

let getStarts dict = Map.filter (curry (snd >> eq 1)) dict |> Map.keys

let parse charNums input = 
    let lines = split "\n" input

    seq { for x in 0..lastidx lines do for y in 0..lastidx lines[x] do yield (x, y), lines[x][y] } 
    |> foldl (addToMap charNums) Map.empty

let part1 input =
    let dict = parse [('X', 1); ('M', 2); ('A', 3); ('S', 4)] input  

    getStarts dict |> sumBy (xmasCount dict)

let ``x-masCount`` (dict: Map<int * int, int>) (x, y) =   
    let matches = [ [0; 0; 2; 2]; [2; 0; 0; 2]; [2; 2; 0; 0]; [0; 2; 2; 0]]

    [x - 1, y - 1; x - 1, y + 1; x + 1, y + 1; x + 1, y - 1]
    |> map dict.TryFind
    |> sequenceOptions
    |> mapO (toList >> eq >> flip List.exists matches >> intB)
    |> withDefault 0

let part2 input =
    let dict = parse [('M', 0); ('A', 1); ('S', 2)] input

    getStarts dict |> sumBy (``x-masCount`` dict)
 