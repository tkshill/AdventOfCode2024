module Day04

let addToMap mapper dict (key, c) = 
    tryFind (fst >> eq c) mapper
    |> mapO (snd >> flip (Map.add key) dict)
    |> withDefault dict

let countXmas dict key =
    let rec isXmas transform (v, key_) =
        match tryFindM (transform key_) dict with
        | Some x when x = v + 1 && x = 4 -> true
        | Some x when x = v + 1 -> isXmas transform (x, (transform key_)) 
        | _ -> false
    
    [ 1, 1; -1, -1; -1, 1; 1, -1; 0, 1; 0, -1; 1, 0; -1, 0 ]
    |> map (mapT inc inc >> uncurry mapT)
    |> sumBy (flip isXmas ((Map.find key dict), key) >> intB)

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

let ``countX-mas`` dict (i, j) =   
    let matches = [ [0; 0; 2; 2]; [2; 0; 0; 2]; [2; 2; 0; 0]; [0; 2; 2; 0]]

    [i - 1, j - 1; i - 1, j + 1; i + 1, j + 1; i + 1, j - 1]
    |> map (flip tryFindM dict)
    |> sequenceOptions
    |> mapO (toList >> eq >> flip List.exists matches >> intB)
    |> withDefault 0


let part2 input =
    let dict = parse [('M', 0); ('A', 1); ('S', 2)] input

    getStarts dict |> sumBy (``countX-mas`` dict)
 