module Day08

let updateMap dict (key, value) =
    Map.change key (function | Some lst -> Some (value :: lst) | None -> Some [value]) dict

let toMap (lines: string array) = 
    seq { for i in 0..lastidx lines do for j in 0..lastidx lines[i] do if lines[i][j] <> '.' then yield lines[i][j], (i, j) } 
    |> foldl updateMap Map.empty

let twinPairs (lst: (int * int) list) = 
    seq { for i in 0..lastidx lst - 1 do for j in i + 1..lastidx lst do yield (lst[i], lst[j]); yield (lst[j], lst[i]) }

let solve (dict: Map<char, list<int * int>>) resolver =
    let defineTranslation (pivotX, pivotY) (x, y) = ((x - pivotX) * -1, (y - pivotY) * -1), (pivotX, pivotY)
    let advancer (deltaX, deltaY) (x, y) = x + deltaX, y + deltaY
    let pairToAntinodes = uncurry defineTranslation >> mapT advancer id >> uncurry iterate >> resolver

    dict.Keys |> collect (flip Map.find dict >> twinPairs >> collect pairToAntinodes) |> distinct |> length

let part1 input =
    let lines = split "\n" input
    let dict = toMap lines
    let inBounds (x, y) = x >= 0 && y >= 0 && x <= lastidx lines && y <= lastidx lines[0]

    solve dict (takeWhile inBounds >> (Seq.tryItem 1 >> mapO Seq.singleton) >> withDefault [])

let part2 input = 
    let lines = split "\n" input
    let inBounds (x, y) = x >= 0 && y >= 0 && x <= lastidx lines && y <= lastidx lines[0]
    let dict = toMap lines

    solve dict (takeWhile inBounds)
