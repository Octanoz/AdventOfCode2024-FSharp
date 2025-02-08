module Program

open System.IO
open System.Text.RegularExpressions
open AdventUtilities

let inputData = InputData()
let day = 3
let fileName = "input"

let filePath = inputData.GetFilePath day fileName

let input = File.ReadAllText filePath

let found =
    seq {
        for m in Regex.Matches(input, @"mul\((?<left>\d{1,3}),(?<right>\d{1,3})\)") do
            let left = m.Groups.["left"].Value |> int
            let right = m.Groups.["right"].Value |> int
            yield left * right
    }
    |> List.ofSeq

type ProcessingState =
    | Active
    | Inactive

let foundConditional =
    seq {
        let rx = @"mul\((?<left>\d{1,3}),(?<right>\d{1,3})\)|(?<toggle>(do(n't)?)\(\))"
        let mutable state = Active

        for m in Regex.Matches(input, rx) do
            if m.Groups["toggle"].Success then
                match m.Groups["toggle"].Value with
                | "do()" -> state <- Active
                | _ -> state <- Inactive
            else
                match state with
                | Active ->
                    let left = m.Groups.["left"].Value |> int
                    let right = m.Groups.["right"].Value |> int
                    yield left * right
                | Inactive -> ()
    }
    |> List.ofSeq

let partOne = found |> List.sum

let partTwo = foundConditional |> List.sum
