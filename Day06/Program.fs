module Program

open System
open AdventUtilities
open Helpers
open Grid

let inputData = InputData()
let input = inputData.ReadAllLines 6 "example1"

let maxRow = input.Length
let maxCol = input[0].Length

let stringArrayTo2DCharArray (input: string array) =
    Array2D.init maxRow maxCol (fun i j -> if j < input[i].Length then input[i][j] else ' ')

let grid = input |> stringArrayTo2DCharArray

let startPosition (grid: char array2d) =
    seq {
        for row in 0 .. maxRow - 1 do
            for col in 0 .. maxCol - 1 do
                if grid[row, col] = '^' then
                    yield row, col
    }
    |> Seq.exactlyOne

let nextMove (row, col) dir grid =
    let nb = anyFourNeighbours (row, col)

    let rec loop dir =
        if grid |> isBlocked (row, col) dir then
            let nextDir = changeDirection dir
            loop nextDir
        else
            match dir with
            | Up -> nb[0], Up
            | Right -> nb[1], Right
            | Down -> nb[2], Down
            | Left -> nb[3], Left

    loop dir

let guardPath initialPosition direction grid =
    let rec guardPosition currentPosition dir acc =
        match grid |> isWithinLimits currentPosition with
        | true ->
            let newPosition, newDirection = grid |> nextMove currentPosition dir

            guardPosition newPosition newDirection ((currentPosition, dir) :: acc)
        | false -> acc

    guardPosition initialPosition direction []

let start = startPosition grid
let travelledPath = guardPath start Up grid |> List.distinctBy fst |> List.rev

let partOne =
    printfn "The guard took %i unique steps before leaving the area" travelledPath.Length



let rec storeStates states (currentSet: Set<(Position * Direction)>) =
    match states with
    | [] -> false, currentSet
    | head :: tail ->
        match currentSet.Contains head with
        | true -> true, currentSet
        | _ -> storeStates tail (currentSet |> Set.add head)

let scanRight (row, col) dir (grid: char array2d) =
    match dir with
    | Up -> grid[row, col..]
    | Right -> grid[row.., col]
    | Down -> grid[row, ..col]
    | Left -> grid[..row, col]
    |> Array.exists (fun c -> c = '#')

let causesLoop obstacleLocation grid =
    let rec loopScanLoop currentPos currentDir acc =
        if grid |> isWithinLimits currentPos then
            let newPos, newDir = grid |> nextMove currentPos currentDir

            if newDir = currentDir then
                loopScanLoop newPos newDir acc
            else
                match acc |> storeStates [ currentPos, currentDir; currentPos, newDir ] with
                | false, updatedSet -> loopScanLoop newPos newDir updatedSet
                | true, _ -> true
        else
            false

    let obstacleRow, obstacleCol = obstacleLocation
    grid[obstacleRow, obstacleCol] <- '#'

    let foundLoop = loopScanLoop start Up Set.empty
    grid[obstacleRow, obstacleCol] <- '.'
    foundLoop

let findLoops (stateList: List<(Position * Direction)>) grid =
    stateList
    |> List.pairwise
    |> List.choose (fun ((currentPos, currentDir), (nextPos, _)) ->
        if scanRight currentPos currentDir grid && causesLoop nextPos grid then
            Some nextPos
        else
            None)

let loopsList = findLoops travelledPath grid

let partTwo =
    printfn "There were %i possible loops found in the guard's path" loopsList.Length
