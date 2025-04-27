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

let getSameRegionNeighbours currentPosition (grid: char array2d) =
    let currentValue = grid |> valueAt currentPosition

    anyFourNeighbours currentPosition
    |> List.filter (fun (r, c) -> grid |> isWithinLimits (r, c) && grid |> valueAt (r, c) = currentValue)

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

    regionDictionary |> updateDictionaryList currentRegion.Id currentRegion

let mapGrid (grid: char array2d) =
    for row in 0 .. grid |> Array2D.length1 |> (+) -1 do
        for col in 0 .. grid |> Array2D.length2 |> (+) -1 do
            if not <| visited[row, col] then
                mapRegion (row, col) grid

mapGrid map

let partOneResult =
    regionDictionary.Values
    |> Seq.sumBy (fun regionList -> regionList |> List.sumBy (fun region -> region.Plots.Length * region.Sides))

printfn $"Part 1 - Total Fence Price: {partOneResult}"

// PART 2
let parse input =
    [ for rn, r in input |> Seq.indexed do
          for cn, c in r |> Seq.indexed -> c, (rn, cn) ]

let plantLocations = parse input

let locationsByPlants =
    plantLocations
    |> List.groupBy fst
    |> List.map (fun (plant, locs) -> plant, locs |> List.map snd)

let dist a b = abs (a - b)

let nextToEachOther one other =
    match one, other with
    | (r1, c1), (r2, c2) when r1 = r2 && dist c1 c2 = 1 -> true
    | (r1, c1), (r2, c2) when c1 = c2 && dist r1 r2 = 1 -> true
    | _ -> false

let rec merge regions =
    let mergeCandidate =
        regions
        |> Seq.indexed
        |> Seq.map (fun (i, region) ->
            region,
            regions
            |> List.skip (i + 1)
            |> List.filter (fun other -> List.allPairs region other |> List.exists (fun (a, b) -> nextToEachOther a b)))
        |> Seq.tryFind (fun (_, neighbourRegions) -> neighbourRegions.Length > 0)

    match mergeCandidate with
    | None -> regions
    | Some(region, mergeRegions) ->
        let toMerge = region :: mergeRegions
        let merged = toMerge |> List.collect id
        let rest = regions |> List.except toMerge
        merge (merged :: rest)

let regionsByPlants =
    locationsByPlants
    |> List.map (fun (plant, locations) -> plant, locations |> List.map List.singleton |> merge)

let area region = region |> Seq.length

let nbCorners region location =
    let exists loc = region |> Seq.contains loc
    let r, c = location

    [
      // Outer corners
      if (r, c + 1) |> exists |> not && (r - 1, c) |> exists |> not then
          1
      else
          0
      if (r, c + 1) |> exists |> not && (r + 1, c) |> exists |> not then
          1
      else
          0
      if (r, c - 1) |> exists |> not && (r - 1, c) |> exists |> not then
          1
      else
          0
      if (r, c - 1) |> exists |> not && (r + 1, c) |> exists |> not then
          1
      else
          0

      // Inner corners
      if (r, c + 1) |> exists && (r + 1, c) |> exists && (r + 1, c + 1) |> exists |> not then
          1
      else
          0
      if (r, c - 1) |> exists && (r + 1, c) |> exists && (r + 1, c - 1) |> exists |> not then
          1
      else
          0
      if (r, c + 1) |> exists && (r - 1, c) |> exists && (r - 1, c + 1) |> exists |> not then
          1
      else
          0
      if (r, c - 1) |> exists && (r - 1, c) |> exists && (r - 1, c - 1) |> exists |> not then
          1
      else
          0 ]
    |> List.sum

let nbSides region =
    region |> List.map (nbCorners region) |> List.sum

let partTwoResult =
    regionsByPlants
    |> List.map snd
    |> List.collect id
    |> List.sumBy (fun region -> area region * nbSides region)

printfn $"Part 2 - Total Fence Price (Bulk Discount): {partTwoResult}"
