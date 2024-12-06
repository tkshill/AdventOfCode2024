[<AutoOpen>]
module Utils

open FParsec
open FParsec.Pipes
open System.Text.RegularExpressions
open FSharpx.Collections
open System

let runParser parser input =
    match run parser input with
    | Success(result, _, _) -> result
    | Failure(errorMsg, _, _) -> 
        failwith $"Parser Error: {errorMsg}"

let runParserWithState parser state input =
    match runParserOnString parser state "" input with
    | Success(result, _, _) -> result
    | Failure(errorMsg, _, _) -> 
        failwith $"Parser Error: {errorMsg}"

let normalizeDay day = 
    if day < 10 then "0" + day.ToString() else day.ToString()

let lastidx s = Seq.length s - 1

let mul = (*)

let thunk s = fun () -> s 

let boolToInt = function true -> 1 | _ -> 0

let apply f x = f x

let flip f = fun x y -> f y x
let ignore2 _ _ = ()

let spread f (a, b) = f a, f b

let toIntC c = c |> Seq.toArray |> String |> int

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

let log (value: 'T) =
    printfn "%A" value
    value

let logF message f a =
    printfn $"{message} {f a}"
    a

let withEffect f a =
    f a
    a

let regMatch pattern s = Regex.Matches(s, pattern)

let split (pattern: string) (s: string) = 
    s.Split([|pattern|], System.StringSplitOptions.RemoveEmptyEntries||| System.StringSplitOptions.TrimEntries )


//let not (f: 'a -> bool) = fun (x: 'a) -> not (f x)


[<AutoOpen>]
module Tuple =

    let flipT (a, b) = (b, a)

    let mapT f f2 (a, b) = (f a, f2 b)

    let mapFst f (a, b) = f a, b

    let mapSnd f (a, b) = a, f b

    let foldT f g (a, b) = g (f a) b

    let curry f = fun x y -> f (x, y)

    let curry3 f = fun x y z -> f (x, y, z)

    let ofList = Seq.ofList

    let uncurry f = fun (x, y) -> f x y

    let tupleToSeq (a: 'a, b: 'a) = seq { yield a; yield b; } 


[<AutoOpen>]
module SeqPlus =

    let append = Seq.append

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

    let item = Seq.item

    let pick = Seq.pick

    let seqmax = Seq.max

    let seqmin = Seq.min

    let pairwise = Seq.pairwise  

    let reduce = Seq.reduce

    let skip = Seq.skip

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

    let indexed = Seq.indexed
    
    let iter = Seq.iter

    let iteri = Seq.iteri

    let takeWhile = Seq.takeWhile

    let toList = Seq.toList

    let transpose = Seq.transpose

    let tryPick = Seq.tryPick

    let tryFindIndex = Seq.tryFindIndex

    let unfold = Seq.unfold

    let single = Seq.singleton

    let empty = Seq.empty

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

    let inc by v = v + by

    let dec by v = v - by

    let intercalate xs yss = intersperse xs yss |> Seq.concat

    let force xs = Seq.fold ignore2 () xs

    let count n = iterate ((+) 1) n

    let enumerate n xs = Seq.zip (count n) xs

    let repeat x = repeatedly (fun () -> x)

    let truncate = Seq.truncate

    let cycle xs = repeat xs |> Seq.concat

    let take n xs =
        enumerate 0 xs |> takeWhile (fun (idx, _) -> idx < n) |> Seq.map snd

    let toSeqRS = ResizeArray.toSeq

    let toArrayRS = ResizeArray.toArray

    let toListRS = ResizeArray.toList

[<AutoOpen>]
module IOHelpers =
    let readAllText (path: string) = System.IO.File.ReadAllText path   
    
    let lines (stream: System.IO.Stream) =
        let sr = new System.IO.StreamReader(stream)
        repeatedly sr.ReadLine |> Seq.takeWhile ((<>) null)

    let slurp = System.IO.File.OpenRead >> lines

let tryFindM = Map.tryFind

let eq = (=)

let noteq = (<>)

let intB = function true -> 1 | _ -> 0