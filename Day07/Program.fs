module Program

open System
open AdventUtilities

let inputData = InputData()
let input = inputData.ReadAllLines 7 "input"

type Operator =
    | Add
    | Multiply
    | Concatenate

let parseLine (line: string) =
    let parts = line.Split ':'
    let target = int64 parts[0]

    let nums =
        parts[1].Split(' ', StringSplitOptions.RemoveEmptyEntries)
        |> Array.map int64
        |> List.ofArray

    target, nums

let concatenate (left: int64) (right: int64) =
    let mutable multiplier = 1L

    while multiplier <= right do
        multiplier <- multiplier * 10L

    left * multiplier + right

let rec cycleOperators currentValue numbers target isPartTwo =
    match numbers with
    | [] -> currentValue = target
    | nextValue :: remainingNumbers ->
        if currentValue > target then
            false
        else if
            //Addition
            currentValue <= Int64.MaxValue - nextValue
            && cycleOperators (currentValue + nextValue) remainingNumbers target isPartTwo
        then
            true
        elif
            //Multiplication
            currentValue <= Int64.MaxValue / nextValue
            && cycleOperators (currentValue * nextValue) remainingNumbers target isPartTwo
        then
            true
        elif isPartTwo then
            //Concatenation
            let concatValue = concatenate currentValue nextValue
            concatValue > 0L && cycleOperators concatValue remainingNumbers target isPartTwo
        else
            false

let validateEquations input isPartTwo =
    let mutable validResults = 0L
    let lockObj = obj ()

    input
    |> Array.Parallel.iter (fun line ->
        let target, nums = parseLine line

        match nums with
        | startValue :: restNums ->
            if cycleOperators startValue restNums target isPartTwo then
                lock lockObj (fun () -> validResults <- validResults + target)
        | _ -> ())

    validResults

let partOne =
    printfn "In part one, the total caliration result is %i" (validateEquations input false)

let partTwo =
    printfn "In part two, the total calibration result is %i" (validateEquations input true)
