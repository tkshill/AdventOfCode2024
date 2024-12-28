module Day12

let updateField (area, lineCoordinates) (x, y) =

    [[-0.5, -0.5; -0.5, 0.5]; [-0.5, 0.5; 0.5, 0.5]; [0.5, 0.5; 0.5, -0.5]; [0.5, -0.5; -0.5, -0.5]] 
    |> map (map (mapT ((+) (float x)) ((+) (float y))) >> Set.ofSeq)
    |> fun newCoords -> (area + 1, append newCoords lineCoordinates)

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

let part1 input = 
    let dict= input |> split "\n" |> stringsToSeq |> Map.ofSeq

    let filterInnerLines = countBy id >> filter (snd >> eq 1) >> map fst

    solve [dict.Keys |> head] [(0, Set.empty)] dict
    |> map (mapSnd (filterInnerLines >> length) >> log)
    |> sumBy (foldT id (*))
