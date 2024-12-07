module Day05

open FParsec
open FParsec.Pipes

let foldPairIntoMap (dict: Map<int, int Set>) (biggerPage, smallerPage) =
    let valuesForBigger = dict.TryFind biggerPage |> withDefault Set.empty |> Set.add smallerPage
    let valuesForSmaller = dict.TryFind smallerPage |> withDefault Set.empty

    dict.Add(biggerPage, valuesForBigger).Add(smallerPage, valuesForSmaller)

let parse = 
    (sepEndBy1 (pint32 .>> %'|' .>>. pint32) %'\n') .>> %'\n' .>>. sepBy1 (sepBy1 pint32 %',') %'\n' 
    |>> mapT (foldl foldPairIntoMap Map.empty) id

let rec isSorted (dict: Map<int, int Set>) = function
    | _ :: [] -> true
    | page :: others when Set.isSubset (Set others) dict[page] |> not -> false
    | _ :: others -> isSorted dict others

let middle items = item (length items / 2) items  // items[items.Length / 2]

let part1 input = 
    let dict, pagesList = runParser parse input
    
    pagesList |> filter (isSorted dict) |> sumBy middle

let refine (dict: Map<int, int Set>) pages  =
    Map.filter (curry (fst >> eq >> flip exists pages)) dict
    |> Map.map (curry (snd >> Set.filter (eq >> flip exists pages)))

let emitInOrder (dict: Map<int, int Set>) = 
    match toList dict.Keys with
    | [] -> None
    | pageNumbers ->
        let allSubPages = Map.fold (fun acc _ v -> Set.union acc v) Set.empty dict 
        let highest :: _ , rest = List.partition (flip Set.contains allSubPages >> not) pageNumbers 

        Some (highest, (refine dict rest))

let part2 input =
    let dict, pagesList = runParser parse input
    
    filter (isSorted dict >> not) pagesList |> sumBy (refine dict >> unfold emitInOrder >> toList >> middle) 
