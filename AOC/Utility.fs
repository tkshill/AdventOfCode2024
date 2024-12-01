[<AutoOpen>]
module Utils

open FParsec
open System.Text.RegularExpressions

let runParser parser input =
    match run parser input with
    | Success(result, _, _) -> result
    | Failure(errorMsg, _, _) -> failwith errorMsg

let flip f = fun x y -> f y x
let ignore2 _ _ = ()

let rec trimEnds sequence =
    match sequence with
    | [||]
    | [| "" |] -> Array.empty
    | x when Array.head x = "" -> trimEnds (Array.tail x)
    | x when Array.last x = "" -> trimEnds (x[.. (Array.length x - 2)])
    | _ -> sequence

[<AutoOpen>]
module Maybe =
    type MaybeBuilder() =
        member _.Bind(m, f) = match m with Some x -> f x | None -> None
        member _.Return(x) = Some x
        member _.ReturnFrom(m) = m
        member this.Zero() = None

    let maybe = MaybeBuilder()

    let mapO = Option.map

    let withDefault = Option.defaultValue

let logValue (value: 'T) =
    printfn "%A" value
    value

let logValueF message f a =
    printfn $"{message} {f a}"
    a

let withEffect f a =
    f a
    a

let regMatch pattern s = Regex.Matches(s, pattern)

let split (pattern: string) (s: string) = 
    s.Split([|pattern|], System.StringSplitOptions.RemoveEmptyEntries||| System.StringSplitOptions.TrimEntries )


let not (f: 'a -> bool) = fun (x: 'a) -> not (f x)


[<AutoOpen>]
module Tuple =

    let flipT (a, b) = (b, a)

    let mapT f f2 (a, b) = (f a, f2 b)

    let mapFst f (a, b) = f a, b

    let mapSnd f (a, b) = a, f b

    let foldT f g (a, b) = g (f a) b

    let curry f = fun x y -> f (x, y)

    let curry3 f = fun x y z -> f (x, y, z)

    let uncurry f = fun (x, y) -> f x y

    let tupleToSeq (a: 'a, b: 'a) = seq { yield a; yield b; } 


[<AutoOpen>]
module SeqPlus =

    let choose = Seq.choose

    let chunkBySize = Seq.chunkBySize

    let collect = Seq.collect

    let concat = Seq.concat

    let contains = Seq.contains

    let containsS (subset: string) (s: System.String) = s.Contains(subset)

    let length = Seq.length

    let exists = Seq.exists

    let find = Seq.find

    let findIndex = Seq.findIndex

    let filter = Seq.filter

    let foldl = Seq.fold

    let forall = Seq.forall

    let groupBy = Seq.groupBy

    let countDistinct (lst: 'a seq) =  Seq.groupBy id lst |> Seq.map (mapT id length)

    let head = Seq.head

    let last = Seq.last

    let map = Seq.map

    let mapi = Seq.mapi

    let seqmax = Seq.max

    let seqmin = Seq.min

    let pairwise = Seq.pairwise  

    let reduce = Seq.reduce

    let sequenceOptions (options: seq<'T option>) : 'T seq option =
        if Seq.exists Option.isNone options then
            None
        else
            Some (options |> Seq.choose id)

    let inline sum (lst: seq< ^a >) : ^a when ^a : (static member (+) : ^a * ^a -> ^a) and ^a : (static member Zero : ^a) =
        Seq.sum lst

    let inline sumBy (f: 'T -> ^b) (lst: seq<'T>) : ^b when ^b : (static member (+) : ^b * ^b -> ^b) and ^b : (static member Zero : ^b) =
        Seq.sumBy f lst

    let sort = Seq.sort

    let sortBy = Seq.sortBy

    let tryFind = Seq.tryFind

    let rev = Seq.rev

    let toArray = Seq.toArray
    
    let iter = Seq.iter

    let takeWhile = Seq.takeWhile

    let toList = Seq.toList

    let transpose = Seq.transpose

    let tryPick = Seq.tryPick

    let unfold = Seq.unfold

    let zip = Seq.zip

    let rec iterate f x =
        seq {
            yield x
            yield! iterate f (f x)
        }

    let rec repeatedly f =
        seq {
            yield f ()
            yield! repeatedly f
        }

    let product xs = Seq.fold (*) 1 xs

    let uncons xs =
        Seq.tryHead xs |> Option.map (fun x -> (x, Seq.tail xs))

    let intersperse x xs =
        match uncons xs with
        | Some(y, zs) ->
            seq {
                yield y

                for z in zs do
                    yield x
                    yield z
            }
        | None -> Seq.empty

    let intercalate xs yss = intersperse xs yss |> Seq.concat

    let force xs = Seq.fold ignore2 () xs

    let count n = iterate ((+) 1) n

    let enumerate n xs = Seq.zip (count n) xs

    let repeat x = repeatedly (fun () -> x)

    let cycle xs = repeat xs |> Seq.concat

    let take n xs =
        enumerate 0 xs |> takeWhile (fun (idx, _) -> idx < n) |> Seq.map snd

[<AutoOpen>]
module IOHelpers =
    let readAllText (path: string) = System.IO.File.ReadAllText path   
    
    let lines (stream: System.IO.Stream) =
        let sr = new System.IO.StreamReader(stream)
        repeatedly sr.ReadLine |> Seq.takeWhile ((<>) null)

    let slurp = System.IO.File.OpenRead >> lines
