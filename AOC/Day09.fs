module Day09

type Item = { Idx: int; Start: int; Amt: int }

let toItem { Amt = amt; Start = start } = function
    | i, c when amt < 0 -> 
        { Idx = i; Start = 0; Amt = charToInt c }

    | i, c -> 
        { Idx = i ; Start = start + amt; Amt = charToInt c }

let parse =
    indexed >> toList >> Seq.scan toItem { Idx = -1; Start = -1; Amt = -1 } 
    >> distinct >> tail >> toList >> List.partition (fun { Idx = i } -> isEven i)

let change space file = 
    let delta = space.Amt - file.Amt

    if delta = 0 then [{ Idx = file.Idx; Start = space.Start; Amt = space.Amt }], [], []

    elif delta > 0 then 
        [{ Idx = file.Idx; Start = space.Start; Amt = file.Amt }], [{ space with Start = space.Start + file.Amt; Amt = delta }], []

    else [{ Idx = file.Idx; Start = space.Start; Amt = space.Amt }], [], [{ file with Amt = delta * -1; }]

let rec optimize = function
    | changes, [], files -> changes @ files

    | changes, _, [] -> changes

    | changes, space :: _, ((file :: _) as files) when space.Start > file.Start -> changes @ files

    | changes, space :: spaces, (file :: files) -> 
        let newchanges, newspaces, newfiles = change space file

        optimize (newchanges @ changes, newspaces @ spaces, newfiles @ files)

let rec resolver total = function 
    | { Amt = 0 } -> total

    | ({ Idx = idx; Amt = amt; Start = start} as item) -> 
        let newtotal = total + ((bigint idx / bigint 2) * bigint start)
 
        resolver (newtotal) { item with Start = start + 1; Amt = amt - 1 } 

let part1 (input: string) =
    let files, spaces = parse input

    optimize ([], spaces, List.rev files) 
    |> sortBy (fun { Start = i } -> i) |> sumBy (resolver (bigint 0) )

let findSmalls spaces file = 
    List.takeWhile (fun { Amt = amt } -> amt < file.Amt) spaces

let change2 space file =
    let change = { Idx = file.Idx; Start = space.Start; Amt = file.Amt }

    if space.Amt - file.Amt = 0 then [change], [], []
    
    else [change], [{ space with Start = space.Start + file.Amt; Amt = space.Amt - file.Amt }], []

let rec optimize2 = function
    | changes, [], files -> changes @ files

    | changes, _, [] -> changes

    | changes, space :: _, ((file :: _) as files) when space.Start > file.Start -> changes @ files

    | changes, spaces, (file :: files)
        when findSmalls spaces file = spaces -> optimize2 (file :: changes, spaces, files)

    | changes, spaces, (file :: files) -> 
        let smalls = findSmalls spaces file
        let space :: spaces = List.skip smalls.Length spaces
        let newchanges, newspaces, newfiles = change2 space file

        optimize2 (newchanges @ changes, smalls @ newspaces @ spaces, newfiles @ files)

let part2 input =
    let files, spaces = parse input

    iter (fun i -> if i.Amt = 0 then printfn "%A" i) files

    optimize2 ([], spaces, List.rev files)
    |> sortBy (fun { Start = i } -> i) 
    //|> logF toList 
    |> sumBy (resolver (bigint 0) )