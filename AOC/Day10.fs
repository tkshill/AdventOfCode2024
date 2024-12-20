module Day10

let parse = split "\n" >> stringsToSeq >> map (mapT id charToInt) >> Map.ofSeq

let neighbours dict (x, y) = 
    [1, 0; 0, 1; -1, 0; 0, -1] 
    |> List.map (mapT (inc x) (inc y)) 
    |> List.filter (flip Map.tryFind dict >> eq (Some (dict[x, y] + 1))) 

let available dict node visited  = 
    neighbours dict node |> List.filter (not << flip Set.contains visited)

let rec dfs dict visited count = function
    | [] -> count
    | node :: stack ->
        let newNodes = available dict node visited
        let newCount = if dict[node] = 9 then count + 1 else count

        dfs dict (visited.Add node) newCount (newNodes @ stack)

let solve solver = 
    Map.filter (curry (snd >> eq 0)) >> Map.keys >> sumBy (List.singleton >> solver)

let part1 input =
    let dict = parse input

    solve (dfs dict Set.empty 0) dict

let rec dfs2 (dict: Map<int * int, int>) count = function
    | [] -> count
    | node :: stack ->
        if dict[node] = 9 then dfs2 dict (count + 1) stack
        
        else dfs2 dict count (neighbours dict node @ stack)

let part2 input = 
    let dict = parse input

    solve (dfs2 dict 0) dict
