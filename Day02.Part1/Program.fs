module Program

open System.IO
open AdventUtilities

let inputData = InputData()
let day = 2
let fileName = "input"

let filePath = inputData.GetFilePath day fileName

let input = File.ReadAllLines filePath |> List.ofArray

let parse (lines: string list) =
    lines |> List.map (fun s -> s.Split() |> Seq.map int |> List.ofSeq)

let withinLimits (num1, num2) =
    let diff = abs (num1 - num2)
    diff >= 1 && diff <= 3

let isIncreasing (num1, num2) =
    num2 > num1 && withinLimits (num1, num2)

let isDecreasing (num1, num2) =
    num2 < num1 && withinLimits (num1, num2)


let isValidList nums =
    let pairs = nums |> List.pairwise

    let allIncreasing = pairs |> List.forall isIncreasing
    let allDecreasing = pairs |> List.forall isDecreasing

    allIncreasing || allDecreasing

let validCount =
    input |> parse |> List.filter (fun l -> isValidList l) |> List.length

printfn $"Valid lists = {validCount}"
