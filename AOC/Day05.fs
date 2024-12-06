module Day05

type OrderTree = MaxValue of OrderTree seq | Node of int * OrderTree seq

let rec foldTree ot (left, right) =
    match ot with
    | MaxValue empty -> 
        Node (right, empty) |> single |> curry Node left |> single |> MaxValue
