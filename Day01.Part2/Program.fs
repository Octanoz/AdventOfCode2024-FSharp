module Program

open System.IO
open AdventUtilities

let inputData = InputData()
let day = 1
let fileName = "example1"

let filePath = inputData.GetFilePath day fileName

let example = File.ReadAllLines filePath |> List.ofArray

let parse (lines: string list) =
    lines
    |> List.map (fun line -> line.Split("   ") |> Seq.map int |> Seq.toList)
    |> List.transpose

let [first;second] = parse example

let numberCounts = second |> List.countBy id |> Map.ofList

let counts = [ for element in first -> (element, numberCounts |> Map.tryFind element |> Option.defaultValue 0) ]
let result = counts |> List.map (fun (a,b) -> a * b) |> List.sum

printfn $"{result}"