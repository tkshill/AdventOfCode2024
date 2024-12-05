module Day04

let addToMap charNums (dict: Map<int * int, int>) (key, letter) = 

    tryFind (fst >> eq letter) charNums
    |> mapO (snd >> curry dict.Add key)
    |> withDefault dict

let isXmas (dict: Map<int * int, int>) key transform =
    let isXmas_  = function
        | newKey, Some lastVal when dict.TryFind newKey = Some (lastVal + 1) -> Some ((), (transform newKey, dict.TryFind newKey))
        | _ -> None
    
    Seq.unfold isXmas_ (transform key, dict.TryFind key) |> Seq.truncate 3 |> Seq.length |> eq 3

let xmasCount dict key =

    [ 1, 1; -1, -1; -1, 1; 1, -1; 0, 1; 0, -1; 1, 0; -1, 0 ]
    |> map (mapT inc inc >> uncurry mapT)
    |> sumBy (isXmas dict key >> intB)

let getStarts dict = Map.filter (curry (snd >> eq 1)) dict |> Map.keys

let parse charNums input = 
    let inputArray = split "\n" input

    seq { for x in 0..lastidx inputArray do for y in 0..lastidx inputArray[x] do yield (x, y), inputArray[x][y] } 
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
 