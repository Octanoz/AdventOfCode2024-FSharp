module Program

open System
open AdventUtilities
open Helpers
open Grid

let inputData = InputData()
let input = inputData.ReadAllLines 8 "input"

let grid = stringArrayTo2DCharArray input

let antennaMap =
    grid
    |> Array2D.mapi (fun i j v -> if v <> '.' then Some(v, (i, j)) else None)
    |> Seq.cast<(char * Position) option>
    |> Seq.choose id
    |> Seq.groupBy fst
    |> Map.ofSeq
    |> Map.map (fun _ v -> v |> Seq.map snd |> List.ofSeq)

let antinodesList =
    [ for antennaList in antennaMap.Values do
          for i in 0 .. antennaList.Length - 1 do
              let current = antennaList[i]

              for j in (i + 1) .. antennaList.Length - 1 do
                  let other = antennaList[j]
                  let difference = diffPair current other
                  let before = movePair current difference //movePair adds tuple a and b together
                  let after = diffPair other difference //diffPair subtracts b from a

                  if grid |> isWithinLimits before then
                      yield before

                  if grid |> isWithinLimits after then
                      yield after ]

let antinodesCount = antinodesList |> List.distinct |> List.length

let partOne =
    printfn $"In part one, the total amount of antinodes was {antinodesCount}."

let rec yieldHarmonics grid (moveFunc: int * int -> int * int) current =
    seq {
        if grid |> isWithinLimits current then
            yield current
            yield! yieldHarmonics grid moveFunc (moveFunc current)
    }

let harmonicsAntinodes =
    [ for antennaList in antennaMap.Values do
          for i in 0 .. antennaList.Length - 1 do
              let first = antennaList[i]

              for j in (i + 1) .. antennaList.Length - 1 do
                  let second = antennaList[j]
                  let difference = diffPair first second

                  yield! yieldHarmonics grid (movePair difference) (movePair difference first)
                  yield! yieldHarmonics grid (fun x -> diffPair x difference) (diffPair second difference) ]

let combinedList =
    (antennaMap.Values |> Seq.collect id |> List.ofSeq) @ harmonicsAntinodes
    |> List.distinct

let partTwo =
    printfn "In part two, there were %i antinodes in total." combinedList.Length
