module Day10

let parse = 
    split "\n" >> stringsToSeq >> map (mapT id charToInt) >> Map.ofSeq

let neighbours dict (x, y) = 
    
    [1, 0; 0, 1; -1, 0; 0, -1] 
    |> List.map (mapT (inc x) (inc y)) 
    |> List.filter (flip Map.tryFind dict >> eq (Some (dict[x, y] + 1))) 

let rec dfs dict visited count = function
    | [] -> count
    | node :: rest ->
        let available = neighbours dict node |> filter (not << flip Set.contains visited) |> toList
        let newCount = if dict[node] = 9 then count + 1 else count

        dfs dict (visited.Add node) newCount (available @ rest)


let part1 input =
    let dict = parse input

    dict 
    |> Map.filter (curry (snd >> eq 0)) 
    |> Map.keys 
    |> sumBy (List.singleton >> dfs dict Set.empty 0)

let rec dfs2 (dict: Map<int * int, int>) count = function
    | [] -> count

    | node :: rest -> //printfn $"node: {node}, value: {dict[node]} rest: {rest} count: {count}"
        if dict[node] = 9 then dfs2 dict (count + 1) rest
        
        else dfs2 dict count (neighbours dict node @ rest)

let part2 input = 
    let dict = parse input

    dict 
    |> Map.filter (curry (snd >> eq 0)) 
    |> Map.keys 
    |> sumBy (List.singleton >> dfs2 dict 0)