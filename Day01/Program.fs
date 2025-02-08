open System.IO
open AdventUtilities

let inputData = InputData()

let filePath = inputData.GetFilePath 1 "input"

let example = File.ReadAllLines filePath |> List.ofArray

let parse (lines: string list) =
    lines
    |> List.map (fun line -> line.Split("   ") |> Seq.map int |> Seq.toList)
    |> List.transpose

let [ first; second ] = parse example

let list1 = first |> List.sort
let list2 = second |> List.sort

let partOne = List.map2 (fun x y -> abs (x - y)) list1 list2 |> List.sum

let numberCounts = second |> List.countBy id |> Map.ofList

let counts =
    [ for element in first -> (element, numberCounts |> Map.tryFind element |> Option.defaultValue 0) ]

let partTwo = counts |> List.map (fun (a, b) -> a * b) |> List.sum

printfn $"list1: {first}\nlist2: {second}"
printfn $"Part One: {partOne}"
printfn $"Part Two: {partTwo}"
