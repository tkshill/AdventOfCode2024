module Day05

open FParsec
open FParsec.Pipes

let folder (dict: Map<int, int Set>) (k, v) =
    let newK = dict.TryFind k |> withDefault Set.empty |> Set.add v
    let newv = dict.TryFind v |> withDefault Set.empty

    dict.Add(k, newK).Add(v, newv)

let parse = 
    (sepEndBy1 (pint32 .>> %'|' .>>. pint32) %'\n') .>> %'\n' .>>. sepBy1 (sepBy1 pint32 %',') %'\n' 
    |>> mapT (foldl folder Map.empty) id

let rec isSorted (dict: Map<int, int Set>) = function
    | _ :: [] -> true
    | x :: xs when Set.isSubset (Set xs) dict[x] |> not -> false
    | _ :: xs -> isSorted dict xs

let middle items = item (length items / 2) items  // items[items.Length / 2]

let part1 input = 
    let dict, pages = runParser parse input
    
    pages |> filter (isSorted dict) |> sumBy middle

let refine (dict: Map<int, int Set>) items  =
    Map.filter (curry (fst >> eq >> flip exists items)) dict
    |> Map.map (curry (snd >> Set.filter (eq >> flip exists items)))

let comparer (dict: Map<int, int Set>) = 
    match toList dict.Keys with
    | [] -> None
    | items ->
        let allDeps = Map.fold (fun acc _ v -> Set.union acc v) Set.empty dict 
        let highest :: _ , rest = List.partition (flip Set.contains allDeps >> not) items 

        Some (highest, (refine dict rest))

let part2 input =
    let dict, pages = runParser parse input
    
    filter (isSorted dict >> not) pages |> sumBy (refine dict >> unfold comparer >> toList >> middle) 
