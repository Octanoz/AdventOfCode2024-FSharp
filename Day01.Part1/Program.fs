open System.IO
open AdventUtilities

let inputData = InputData()
let day = 1
let fileName = "input"

let filePath = inputData.GetFilePath day fileName

let example = File.ReadAllLines filePath |> List.ofArray

let parse (lines: string list) =
    lines
    |> List.map (fun line -> line.Split("   ") |> Seq.map int |> Seq.toList)
    |> List.transpose

let [first;second] = parse example

let list1 = first |> List.sort
let list2 = second |> List.sort

let totalSum =
    List.map2 (fun x y -> abs (x - y)) list1 list2
    |> List.sum


printfn $"list1: {first}\nlist2: {second}"
printfn $"{totalSum}"