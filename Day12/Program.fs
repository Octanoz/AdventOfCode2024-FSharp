open System
open AdventUtilities
open Helpers
open Grid
open System.Collections.Generic

let inputData = InputData()

let input = inputData.ReadAllLines 12 "input"

type Region =
    { Id: char
      Plots: (int * int) list
      Sides: int }

let map = input |> stringArrayTo2DCharArray

let visited = falseGrid (map.GetLength 0) (map.GetLength 1)

let getNeighbours (currentPosition: Position) (grid: char array2d) =
    anyFourNeighbours currentPosition
    |> List.map (fun (r, c) ->
        if grid |> isWithinLimits (r, c) then
            grid[r, c], (r, c)
        else
            '.', (0, 0))

let getSameRegionNeighbours currentPosition (grid: 'T array2d) =
    let currentValue = grid |> valueAt currentPosition

    anyFourNeighbours currentPosition
    |> List.filter (fun (r, c) -> grid |> isWithinLimits (r, c) && grid |> valueAt (r, c) = currentValue)

let updateDictionary (dict: Dictionary<char, Region list>) (key: char) (value: Region) =
    match dict.TryGetValue key with
    | true, cachedValue -> dict[key] <- value :: cachedValue
    | false, _ -> dict.Add(key, [ value ])

let regionDictionary = Dictionary<char, Region list>()

let mapRegion (row, col) (grid: char array2d) =
    let regionQueue = Queue()
    regionQueue.Enqueue((row, col))

    let rec bfs coordinates =
        match regionQueue.Count with
        | 0 -> coordinates
        | _ ->
            let currentPosition = regionQueue.Dequeue()
            let currentRow, currentCol = currentPosition

            if visited |> valueAt currentPosition |> not then
                visited[currentRow, currentCol] <- true
                let sameRegion = grid |> getSameRegionNeighbours currentPosition
                let sides = 4 - sameRegion.Length

                for nb in sameRegion do
                    regionQueue.Enqueue nb

                bfs ((sides, currentPosition) :: coordinates)
            else
                bfs coordinates

    let regionCoordinates = bfs []

    let currentRegion =
        { Id = grid |> valueAt (row, col)
          Plots = regionCoordinates |> List.map snd
          Sides = regionCoordinates |> List.sumBy fst }

    updateDictionary regionDictionary currentRegion.Id currentRegion


let mapGrid (grid: char array2d) =
    for row in 0 .. grid |> Array2D.length1 |> (+) -1 do
        for col in 0 .. grid |> Array2D.length2 |> (+) -1 do
            if not <| visited[row, col] then
                mapRegion (row, col) grid

let countSides (grid: char[,]) (row, col) =
    let currentLetter = grid[row, col]

    4
    - (grid
       |> getNeighbours (row, col)
       |> List.filter (fun (letter, _) -> letter = currentLetter)
       |> List.length)

// First, let's add our new helper functions at the top of the file
let sumSides id =
    regionDictionary[id] |> List.sumBy (fun region -> region.Sides)

let plotsCount id =
    regionDictionary[id] |> List.sumBy (fun region -> region.Plots.Length)

let perRegion id =
    regionDictionary[id]
    |> List.map (fun region -> region.Plots.Length, region.Sides)

let regionTuples =
    regionDictionary
    |> Seq.map (fun kvp -> kvp.Key, plotsCount kvp.Key, sumSides kvp.Key)
    |> List.ofSeq

// Now we can remove the repeated calculations and use our helpers

let debugOne =
    for KeyValue(id, regionList) in regionDictionary do
        if regionList.Length = 0 then
            printfn $"There should have been plots here but apparently region {id} exists without any plots..."
        elif regionList.Length = 1 then
            printf $"Region {id} is a single region with {regionList[0].Plots.Length} plots and "
            printf $"{regionList[0].Sides} sides\n"
        else
            printfn
                $"Region {id} consists of {regionList |> List.length} separate regions which have the following amount of plots and sides:"

            printfn $"%A{perRegion id}"
            printfn $"Which makes a grand total of {plotsCount id} plots and {sumSides id} sides"

let resultOne =
    mapGrid map

    regionDictionary.Values
    |> Seq.sumBy (fun regionList -> regionList |> List.sumBy (fun region -> region.Plots.Length * region.Sides))

let partOne = printfn $"For part 1, the total fence price is {resultOne}"

// Instead of counting all sides on each edge plot combine those that are in sequence

// An outer corner is a coordinate that has 2 neighbouring cells of the same kind as itself
// An inner corner will be the first coordinate from any corner that is surrounded by the same kind of cells as itself.
// Should a single width row count as adding 1 corner just like the bigger ones or would it be more correct if it adds 2?

// If possible start in a single cell width row at furthest cell, aka a pillar
// Collect all the 'pillars' in a collection
// A regular side has only one neighbour that is different from itself.

// Start traversal with 2 if stsrting from a pillar, otherwise start with 1
// Find the first cell with 2 neighbours or 0 neighbours which will be another corner.

let isPillar (row, col) (grid: char array2d) =
    grid |> getSameRegionNeighbours row, col |> List.length = 1

let rec findCorner (row, col) (grid: char array2d) =
    if isPillar row col grid then
        Some(row, col)
    else
        let neighbours = grid |> getSameRegionNeighbours row, col

        match List.tryFind (fun (_, n) -> n <> grid.[row, col]) neighbours with
        | Some(_, 2) -> findCorner row col grid
        | _ -> None
        |> Option.map (fun (r, c) -> r + 1, c)

let rec countPillarsAndCorners (row, col) (grid: char array2d) =
    let pillars = List.filter isPillar grid |> List.length

    let corners =
        findCorner row col grid |> Option.map (fun (_, _) -> 1) |> Option.defaultValue 0

    pillars + corners

let rec countPillarsAndCornersInGrid (grid: char array2d) =
    grid
    |> Array2D.iteri (fun r c -> printfn "%A" (countPillarsAndCorners r c grid))
// Day10/Program.fs
