module Day04

let addToMap mapper (dict: Map<int * int, int>) (key, c) = 

    tryFind (fst >> eq c) mapper
    |> mapO (snd >> curry dict.Add key)
    |> withDefault dict

let isXmas (dict: Map<int * int, int>) key transform =
    let rec isXmas_ newKey lastVal =
        match dict.TryFind newKey with
        | Some x when x = lastVal + 1 && x = 4 -> true
        | Some x when x = lastVal + 1 -> isXmas_ (transform newKey) x
        | _ -> false
    
    isXmas_ (transform key) dict[key]

let countXmas dict key =

    [ 1, 1; -1, -1; -1, 1; 1, -1; 0, 1; 0, -1; 1, 0; -1, 0 ]
    |> map (mapT inc inc >> uncurry mapT)
    |> sumBy (isXmas dict key >> intB)

let getStarts dict = Map.filter (curry (snd >> eq 1)) dict |> Map.keys

let parse mapping input = 
    let inputArray = split "\n" input

    seq { 
        for i in 0..lastidx inputArray do
            for j in 0..lastidx inputArray[i] do
                yield (i, j), inputArray[i][j] 
    } |> foldl (addToMap mapping) Map.empty

let part1 input =
    let dict = parse [('X', 1); ('M', 2); ('A', 3); ('S', 4)] input  

    getStarts dict |> sumBy (countXmas dict)

let ``countX-mas`` (dict: Map<int * int, int>) (i, j) =   
    let matches = [ [0; 0; 2; 2]; [2; 0; 0; 2]; [2; 2; 0; 0]; [0; 2; 2; 0]]

    [i - 1, j - 1; i - 1, j + 1; i + 1, j + 1; i + 1, j - 1]
    |> map dict.TryFind
    |> sequenceOptions
    |> mapO (toList >> eq >> flip List.exists matches >> intB)
    |> withDefault 0


let part2 input =
    let dict = parse [('M', 0); ('A', 1); ('S', 2)] input

    getStarts dict |> sumBy (``countX-mas`` dict)
 