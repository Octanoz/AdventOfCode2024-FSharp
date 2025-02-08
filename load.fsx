#r "AdventUtilities/bin/Debug/net9.0/AdventUtilities.dll"

open System.IO
open AdventUtilities

let inputData = InputData()

let readFile day fileName method =
    let filePath = inputData.GetFilePath day fileName
    match method with
    | "lines" ->
        let lines = inputData.ReadAllLines day fileName
        printfn $"File Content (Lines): %A{lines}"
    | "text" ->
        let text = inputData.ReadAllText day fileName
        printfn $"File Content (Text): %s{text}"
    | "stream" ->
        let stream = inputData.ReadWithStreamReader day fileName
        printfn $"File Content (Stream): %A{Seq.toList stream}"
    | _ ->
        printfn $"Unknown method: %s{method}"