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

        | node :: stack, fstField :: restFields ->
            let newStack = List.distinct (findNeighbours dict node @ stack) 
            let newFields = updateField fstField node :: restFields

            solve newStack newFields (dict.Remove node)

let filterInnerLines (lineCoordinates) = 
    lineCoordinates 
    |> countBy id 
    |> filter (snd >> eq 1)
    |> map fst


// let areaAndLines input = 
//     let dict= input |> split "\n" |> stringsToSeq |> Map.ofSeq
   
//     solve [dict.Keys |> head] [(0, Set.empty)] dict
//     |> map (mapSnd (filiterInnerLines))
//     |> toList

let part1 input = 
    let dict= input |> split "\n" |> stringsToSeq |> Map.ofSeq

    solve [dict.Keys |> head] [(0, Set.empty)] dict
    |> map (mapSnd (filterInnerLines))

    |> map (mapSnd length)
    |> sumBy (log >> foldT id (*))

// let findSides lineCoords =
//     let rec findSides' count cuurentlyHorizontal = function
//         | [] -> count
//         | head :: tail ->
//             let (next :: _), rest = List.partition (Set.intersect head >> length >> eq 1) tail
//             let coord1 :: coord2 :: _ = next
//             let isHorizontal = fst coord1 = fst coord2
//             let stillHorizontal = isHorizontal = cuurentlyHorizontal

//             findSides' (count + toIntB stillHorizontal) isHorizontal (next :: rest)
    
//     let (first :: rest) = lineCoords
//     let coord1 :: coord2 :: _ = Set.toList first
//     let isHorizontal= fst coord1 = fst coord2

//     findSides' 1 isHorizontal (first :: rest)


// let part2 input =
//     let dict= input |> split "\n" |> stringsToSeq |> Map.ofSeq

//     solve [dict.Keys |> head] [(0, Set.empty)] dict
//     |> map (mapSnd (filiterInnerLines))
    // |> map (mapSnd length)
    // |> sumBy (log >> foldT id (*))


