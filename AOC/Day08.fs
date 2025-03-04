module Day08

let updateMap dict (key, value) =
    Map.change key (function | Some lst -> Some (value :: lst) | None -> Some [value]) dict

let toMap (lines: string array) = 
    stringsToSeq lines 
    |> map flipT
    |> foldl updateMap Map.empty

let twinPairs (lst: (int * int) list) = 
    seq { 
        for i in 0..lastidx lst - 1 do 
        for j in i + 1..lastidx lst do 
        yield (lst[i], lst[j]); yield (lst[j], lst[i]) }

let defineTranslation (pivotX, pivotY) (x, y) = 
    ((x - pivotX) * -1, (y - pivotY) * -1), (pivotX, pivotY)

let solve (dict: Map<char, list<int * int>>) resolver =
    
    let advancer (deltaX, deltaY) (x, y) = x + deltaX, y + deltaY
    let pairToAntinodes = uncurry defineTranslation >> mapT advancer id >> uncurry iterate >> resolver

    dict.Keys |> collect (flip Map.find dict >> twinPairs >> collect pairToAntinodes) |> distinct |> length

let parse input =
    let lines = split "\n" input

    toMap lines, fun (x, y) -> x >= 0 && y >= 0 && x <= lastidx lines && y <= lastidx lines[0]

let part1 input =
    let dict, boundaryCondition = parse input

    solve dict (takeWhile boundaryCondition >> (tryItem 1 >> mapO singleton) >> withDefault [])

let part2 input = 
    let dict, boundaryCondition = parse input

    solve dict (takeWhile boundaryCondition)
