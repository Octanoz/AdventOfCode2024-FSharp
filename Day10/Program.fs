open AdventUtilities
open Helpers.Grid
open System.Collections.Generic

let inputData = InputData()

let input = inputData.ReadAllLines 10 "input"

let grid = stringArrayTo2DCharArray input |> Array2D.map (fun c -> int (c - '0'))

let maxRow = grid |> Array2D.length1
let maxCol = grid |> Array2D.length2

let isValid pos =
    fst pos >= 0 && fst pos < maxRow && snd pos >= 0 && snd pos < maxCol

let numberSequence = [ 0..9 ]

let findValue (currentPos: Position) = grid[fst currentPos, snd currentPos]

// Valid meaning here that they are +1 the current value and within bounds
let validNeighbours currentPos currentVal =
    match numberSequence |> List.tryFindIndex (fun n -> n = currentVal + 1) with
    | Some index ->
        anyFourNeighbours currentPos
        |> List.filter (fun pos -> pos |> isValid && findValue pos = numberSequence[index])
    | None -> []

let findReachableNinesFromTrailhead trailhead =
    let rec dfs positions uniqueNines =
        match positions with
        | [] -> Set.count uniqueNines
        | current :: rest ->
            let currentValue = findValue current

            match currentValue with
            | 9 -> dfs rest (uniqueNines |> Set.add current)
            | _ ->
                let newPositions = validNeighbours current currentValue
                dfs (newPositions @ rest) uniqueNines

    dfs [ trailhead ] Set.empty

// Find all '0', the trailheads, in the original input data
let trailheads =
    input
    |> Seq.mapi (fun row rows ->
        rows
        |> Seq.mapi (fun col _ -> if rows[col] = '0' then Some(row, col) else None)
        |> Seq.choose id)
    |> Seq.concat
    |> List.ofSeq

let resultOne =
    trailheads
    |> List.map (fun trailhead -> findReachableNinesFromTrailhead trailhead)
    |> List.sum



let findPaths =
    let queue = Queue<Position>() // .NET Queue<T>

    for th in trailheads do
        queue.Enqueue(th)

    let rec bfs score =
        match queue.Count with
        | 0 -> score
        | _ ->
            let current = queue.Dequeue()
            let currentValue = findValue current

            match currentValue with
            | 9 -> bfs (score + 1)
            | _ ->
                for nb in validNeighbours current currentValue do
                    queue.Enqueue(nb)

                bfs score

    bfs 0 // BFS and initial state

let partOne =
    printfn "In part one, the sum of the scores of all trailheads is %i" resultOne

let partTwo =
    printfn "In part two, the score of all paths to the tops is %i" findPaths
