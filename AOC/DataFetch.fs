module DataFetch

open System
open FsHttp.DslCE
open FsHttp.Response
open System.IO
open FsHttp.Request
open FSharp.Configuration

type Settings = AppSettings<"app.config"> 

let fetchData day = 

    let sessionKey = Settings.SessionKey

    printfn $"Fetching input for Day{day} using session key {sessionKey}."

    http {
        GET $"https://adventofcode.com/2024/day/{day}/input"
        Cookie "session" sessionKey
    }
    |> send
    |> toTextAsync 
    |> Async.RunSynchronously

let getInputForDay (day: int) =
    let filePath = Path.Combine("data", $"Day{normalizeDay(day)}.txt")

    if File.Exists(filePath) then
        printfn $"Input file for Day{day} already exists. Reading from local file."
        Some <| File.ReadAllText(filePath)
    else
        printfn $"Input file for Day{day} does not exist locally. Fetching from AoC website."
        
        try
            let input = fetchData day

            File.WriteAllText(filePath, input)
            Some input
        with
        | :? Exception as e ->
            printfn $"Error fetching input for Day{day}: {e.Message}"
            None

let refreshInputForDay day =
    let filePath = Path.Combine("data", $"Day{day}.txt")

    if File.Exists(filePath) then
        File.Delete(filePath)
        printfn $"Input file for Day{day} deleted."
    else
        printfn $"Input file for Day{day} does not exist locally."

    getInputForDay day