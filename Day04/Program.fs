module Program

open System.IO
open AdventUtilities

let inputData = InputData()
let day = 4
let fileName = "input"

let filePath = inputData.GetFilePath day fileName

let input = File.ReadAllLines filePath
let maxRow = input.Length
let maxCol = input[0].Length

let withinLimits (row, col) =
    row >= 0 && row < maxRow && col >= 0 && col < maxCol

let stringArrayTo2DCharArray (input: string array) =
    Array2D.init maxRow maxCol (fun i j -> if j < input[i].Length then input[i][j] else ' ')

let grid = input |> stringArrayTo2DCharArray

let target = [| 'X'; 'M'; 'A'; 'S' |]

let move num =
    [ -1, -1; -1, 0; -1, 1; 0, -1; 0, 1; 1, -1; 1, 0; 1, 1 ][num]

let checkCorners (row, col) =
    let corners =
        [ row - 1, col - 1; row - 1, col + 1; row + 1, col - 1; row + 1, col + 1 ]

    [ for (r, c) in corners do
          match withinLimits (r, c) with
          | true -> yield grid[r, c]
          | false -> yield '.' ]

let xmasSequences (row, col) (target: char array) (grid: char[,]) : int =
    let xmasArray = [| 'X'; '.'; '.'; '.' |]

    let matches =
        seq {
            for i in 0..7 do
                let (moveRow, moveCol) = move i
                Array.fill xmasArray 1 3 '.'

                for j in 1..3 do
                    let (newRow, newCol) = (moveRow * j + row, moveCol * j + col)

                    if withinLimits (newRow, newCol) then
                        xmasArray[j] <- grid[newRow, newCol]

                if xmasArray = target then
                    yield 1
        }
        |> Seq.sum

    matches

let collectStarters (grid: char[,]) letter =
    grid
    |> Array2D.mapi (fun i j v -> if v = letter then Some(i, j) else None)
    |> Seq.cast<(int * int) option>
    |> Seq.choose id
    |> List.ofSeq

let findChristmas (grid: char[,]) =
    let starters = collectStarters grid 'X'

    starters |> List.map (fun x -> xmasSequences x target grid) |> List.sum

let isCross =
    function
    | [ 'M'; 'M'; 'S'; 'S' ]
    | [ 'M'; 'S'; 'M'; 'S' ]
    | [ 'S'; 'M'; 'S'; 'M' ]
    | [ 'S'; 'S'; 'M'; 'M' ] -> true
    | _ -> false

let findXmas (grid: char[,]) =
    let starters = collectStarters grid 'A'

    starters
    |> List.map (fun elemA -> checkCorners elemA)
    |> List.filter isCross
    |> List.length

let partOne = grid |> findChristmas

let partTwo = grid |> findXmas

printfn "Sequences found in part one: %i" partOne
printfn "Sequences found in part two: %i" partTwo
