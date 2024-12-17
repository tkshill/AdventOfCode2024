module Day12

let updateField (area, lineCoordinates) (x, y) =

    [[-0.5, -0.5; -0.5, 0.5]; [-0.5, 0.5; 0.5, 0.5]; [0.5, 0.5; 0.5, -0.5]; [0.5, -0.5; -0.5, -0.5]] 
    |> map (map (mapT ((+) (float x)) ((+) (float y))) >> Set.ofSeq)
    |> fun newCoords -> (area + 1, append newCoords lineCoordinates) //|> List.partition (area |> Set.contains) |> map Set.ofList

let findNeighbours (dict: Map<int * int, char>) ((nodeX, nodeY) as node) = 

    [1, 0; 0, 1; -1, 0; 0, -1] 
    |> map (mapT (inc nodeX) (inc nodeY)) 
    |> filter (fun key -> dict.ContainsKey key && dict[key] = dict[node])
    |> toList

let rec solve stack fields (dict: Map<int * int, char>) =
    if dict.IsEmpty then fields
    else
        match stack, fields with 
        | [], _ -> solve [(dict.Keys |> head)] ((0, empty) :: fields) dict

        | node :: stack, field :: fields ->
            let newStack = List.distinct (findNeighbours dict node @ stack) 
            let newFields = updateField field node :: fields

            solve newStack newFields (dict.Remove node)

let filterInnerLines (lineCoordinates) = 
    lineCoordinates |> countBy id |> filter (snd >> eq 1) |> map fst

let part1 input = 
    let dict= input |> split "\n" |> stringsToSeq |> Map.ofSeq

    solve [dict.Keys |> head] [(0, Set.empty)] dict
    |> map (mapSnd (filterInnerLines >> length) >> log)
    |> sumBy (foldT id (*))


let findSides ((head :: tail): list<Set<float * float>>) =
    let rec findSides' count currentlyHorizontal = function
        | [] -> count
        | last :: [] when last = head -> count
        | last :: [] -> findSides' count currentlyHorizontal (last :: [head])
        | head :: tail ->
            //printfn "good0"
            let test, rest = List.partition (Set.intersect head >> length >> greaterThan 0) tail
            if test = [] then failwith $"No intersection found for\nHead: {head}\nTail: {tail}\nCount: {count}\n"
            let next :: hmm = test
            //printfn "good"
            let coord1 :: coord2 :: _ = Set.toList next
            //printfn "good1"
            let isHorizontal = fst coord1 = fst coord2
            let changed = isHorizontal <> currentlyHorizontal

            findSides' (count + toIntB changed) isHorizontal (next :: hmm @ rest)
    
    let coord1 :: coord2 :: _ = Set.toList head
    let isHorizontal= fst coord1 = fst coord2

    findSides' 0 isHorizontal (head :: tail) 


let part2 input =
    let dict= input |> split "\n" |> stringsToSeq |> Map.ofSeq

    solve [dict.Keys |> head] [(0, Set.empty)] dict
    |> map (mapSnd (filterInnerLines >> toList >> findSides))
    |> sumBy (foldT id (*))


