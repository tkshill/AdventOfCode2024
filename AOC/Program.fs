open Argu
open System
open System.Reflection
open DataFetch


// Define CLI argument options
type CLIArgs =
    | [<EqualsAssignment>][<First>] Day of int
    | [<EqualsAssignment>] Part of int
    | [<EqualsAssignment>][<First>] RefreshDay of int
    | [<NoCommandLine>] SessionKey of string
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Day _ -> "Specify the day of the Advent of Code solution to run."
            | Part _ -> "Specify the part (1 or 2) of the day's solution to run."
            | RefreshDay _ -> "Refresh the input data for the specified day."
            | SessionKey _ -> "Specify the session key to use for fetching input data."


let getModule day =
    try
        Some (Type.GetType($"Day{normalizeDay(day)}", throwOnError = true))
    with
    | :? TypeLoadException ->
        printfn $"Module {day} not found."
        None

let getMethod (moduleType: Type) (name: string) =
    try
        Some (moduleType.GetMethod(name))
    with
    | :? TypeLoadException ->
        printfn $"Method {name} not found."
        None

let getMethods (moduleType: Type) parts =
    let methods = moduleType.GetMethods(BindingFlags.Public ||| BindingFlags.Static)

    let whichParts =
        match parts with
        | Some 1 -> containsS "part1"
        | Some 2 -> containsS "part2"
        | None -> fun s -> containsS "part1" s || containsS "part2" s

    methods
    |> map (fun methodInfo -> methodInfo.Name)
    |> filter whichParts 
    |> map (getMethod moduleType)
    |> sequenceOptions

[<EntryPoint>]
let main argv =
    let arguments =  ArgumentParser.Create<CLIArgs>(programName = "AdventOfCode").Parse(argv)

    maybe {

        let! day = arguments.TryGetResult <@ Day @>

        let part = arguments.TryGetResult <@ Part @>

        let! data = getInputForDay day

        let! moduleType = getModule day

        let! methods = getMethods moduleType part

        methods
        |> iter (fun method ->
            printfn $"Running {method.Name} of {day}"
            printfn $"Answer: {method.Invoke(null, [| data |])}"
            ) 
    } |> ignore

    0
