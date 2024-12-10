[<AutoOpen>]
module Utils

open FParsec
open System.Text.RegularExpressions
open FSharpx.Collections
open System

[<AutoOpen>]
module ResizeArray =

    let toSeqRS = ResizeArray.toSeq

    let toArrayRS = ResizeArray.toArray

    let toListRS = ResizeArray.toList

[<AutoOpen>]
module List =

    let rec permutations = function
        | []      -> seq [List.empty]
        | x :: xs -> Seq.collect (insertions x) (permutations xs)
    and insertions x = function
        | []             -> [[x]]
        | (y :: ys) as xs -> (x::xs)::(List.map (fun x -> y::x) (insertions x ys))

[<AutoOpen>]
module Map =

    let tryFindM = Map.tryFind

[<AutoOpen>]
module Math =

    let eq = (=)

    let greaterThan criteria n = n > criteria

    let isEven n = n % 2 = 0

    let lessThan criteria n =  n < criteria

    let mul a b = b * a

    let noteq = (<>)
    
    let charToInt (c: char) = int c - int '0'

[<AutoOpen>]
module Miscellaneous =

    let flip f = fun x y -> f y x


    let ignore2 _ _ = ()

    let normalizeDay day = 
        if day < 10 then "0" + day.ToString() else day.ToString()

    let regMatch pattern s = Regex.Matches(s, pattern)

    let split (pattern: string) (s: string) = 
        s.Split([|pattern|], StringSplitOptions.RemoveEmptyEntries||| StringSplitOptions.TrimEntries )

    let toIntB = function true -> 1 | _ -> 0
    
    let toIntC c = c |> Seq.toArray |> String |> int

    let thunk s = fun () -> s

    let rec trimEnds sequence =
        match sequence with
        | [||]
        | [| "" |] -> Array.empty
        | x when Array.head x = "" -> trimEnds (Array.tail x)
        | x when Array.last x = "" -> trimEnds (x[.. (Array.length x - 2)])
        | _ -> sequence

let apply f x = f x

[<AutoOpen>]
module Maybe =
    type MaybeBuilder() =
        member _.Bind(m, f) = match m with Some x -> f x | None -> None
        member _.Return(x) = Some x
        member _.ReturnFrom(m) = m
        member this.Zero() = None

    let maybe = MaybeBuilder()

    let mapO = Option.map

    let spread f (a, b) = f a, f b

    let traverseM (options: seq<'T option>) : 'T seq option =
        if Seq.exists Option.isNone options then
            None
        else
            Some (options |> Seq.choose id)

    let withDefault = Option.defaultValue

[<AutoOpen>]
module Tuple =

    let toTuple x y = x, y

    let flipT (a, b) = (b, a)

    let fst3 (x, _, _) = x

    let mapT f f2 (a, b) = (f a, f2 b)

    let mapFst f (a, b) = f a, b

    let mapSnd f (a, b) = a, f b

    let foldT f g (a, b) = g (f a) b

    let curry f = fun x y -> f (x, y)

    let curry3 f = fun x y z -> f (x, y, z)    

    let toSeqT (a: 'a, b: 'a) = seq { yield a; yield b; }

    let trd (_, _, x) = x

    let uncurry f = fun (x, y) -> f x y

[<AutoOpen>]
module Seq =

    let append = Seq.append

    let choose = Seq.choose

    let chunkBySize = Seq.chunkBySize

    let collect = Seq.collect

    let concat = Seq.concat

    let contains = Seq.contains

    let containsS (subset: string) (s: String) = s.Contains(subset)

    let distinct = Seq.distinct

    let length = Seq.length

    let exists = Seq.exists

    let find = Seq.find

    let findIndex = Seq.findIndex

    let filter = Seq.filter

    let foldl = Seq.fold

    let forall = Seq.forall

    let groupBy = Seq.groupBy

    let indexed = Seq.indexed

    let isEmpty = Seq.isEmpty

    let item = Seq.item
    
    let iter = Seq.iter

    let iteri = Seq.iteri

    let head = Seq.head

    let last = Seq.last

    let lastidx s = Seq.length s - 1

    let map = Seq.map

    let mapi = Seq.mapi

    let map2 = Seq.map2

    let ofList = Seq.ofList

    let pick = Seq.pick

    let seqmax = Seq.max

    let maxBy = Seq.maxBy

    let minBy = Seq.minBy

    let seqmin = Seq.min

    let pairwise = Seq.pairwise  

    let reduce = Seq.reduce

    let singleton = Seq.singleton

    let skip = Seq.skip

    let inline sum (lst: seq< ^a >) : ^a when ^a : (static member (+) : ^a * ^a -> ^a) and ^a : (static member Zero : ^a) =
        Seq.sum lst

    let inline sumBy (f: 'T -> ^b) (lst: seq<'T>) : ^b when ^b : (static member (+) : ^b * ^b -> ^b) and ^b : (static member Zero : ^b) =
        Seq.sumBy f lst

    let sort = Seq.sort

    let sortBy = Seq.sortBy

    let tryFind = Seq.tryFind

    let rev = Seq.rev

    let toArray = Seq.toArray

    let tail = Seq.tail

    let take = Seq.take

    let takeWhile = Seq.takeWhile

    let toList = Seq.toList

    let transpose = Seq.transpose

    let tryItem = Seq.tryItem

    let tryPick = Seq.tryPick

    let tryFindIndex = Seq.tryFindIndex

    let unfold = Seq.unfold

    let single = Seq.singleton

    let empty = Seq.empty

    let zip = Seq.zip

    let product xs = Seq.fold (*) 1 xs

    let inc by v = v + by

    let dec by v = v - by

    let force xs = Seq.fold ignore2 () xs

    let truncate = Seq.truncate

[<AutoOpen>]
module SeqPlus = 

    let stringsToSeq (lines: string array) = seq { for i in 0..lastidx lines do for j in 0..lastidx lines do yield (i, j), lines[i][j] }

    let countDistinct (lst: 'a seq) =  groupBy id lst |> map (mapT id length)

    let rec iterate f x =
        seq {
            yield x
            yield! iterate f (f x)
        }

    let count n = iterate ((+) 1) n

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

    let partition condition = 
        Seq.fold (fun (a, b) x -> 
            if condition x then (x :: a, b) else (a, x :: b)
        ) ([], [])

    let rec repeatedly f =
        seq {
            yield f ()
            yield! repeatedly f
        }

    let repeat x = repeatedly (fun () -> x)

    let cycle xs = repeat xs |> concat

[<AutoOpen>]
module IO =
    let readAllText (path: string) = System.IO.File.ReadAllText path   
    
    let lines (stream: IO.Stream) =
        let sr = new IO.StreamReader(stream)
        repeatedly sr.ReadLine |> takeWhile ((<>) null)

    let log (value: 'T) =
        printfn "%A" value
        value

    let logF func value =
        printfn $"{func value}"
        value

    let logFM message func value =
        printfn $"{message}{func value}"
        value

    let logM message value =
        printfn "%s: %A" message value
        value

    let slurp = IO.File.OpenRead >> lines

    let withEffect f a =
        f a
        a

[<AutoOpen>]
module Parser =

    let runParserWithState parser state input =
        match runParserOnString parser state "" input with
        | Success(result, _, _) -> result
        | Failure(errorMsg, _, _) -> failwith errorMsg

    let runParser parser input =
        match run parser input with
        | Success(result, _, _) -> result
        | Failure(errorMsg, _, _) -> failwith errorMsg

    let runLineParser parser input = split "\n" input |> map (runParser parser)
        