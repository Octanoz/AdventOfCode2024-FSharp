open System
open AdventUtilities
open Helpers
open System.Collections.Generic

let inputData = InputData()
let input = inputData.ReadAllText 11 "input"
let organizeInput = input.Split ' ' |> Array.map int64

let getDivisor (num: int64) = pown 10L (int num)
let digitCount num = floor (log10 num + 1.0)
let hasEvenDigitCount num = int64 (digitCount num) % 2L = 0
let takeHalf (x: int64) = x / 2L

// Initial cache values
let splitList =
    [ 20L, (2L, 0L)
      24L, (2L, 4L)
      40L, (4L, 0L)
      48L, (4L, 8L)
      80L, (8L, 0L)
      96L, (9L, 6L)
      2024L, (20L, 24L)
      4048L, (40L, 48L)
      8096L, (80L, 96L) ]

let splitCache =
    splitList |> List.map toKeyValue |> Dictionary<int64, int64 * int64>

let splitStone (num: int64) =
    match splitCache.TryGetValue(num) with
    | true, (x, y) -> x, y
    | false, _ ->
        let divisor = int64 <| digitCount (double num) |> takeHalf |> getDivisor
        splitCache.Add(num, (num / divisor, num % divisor))
        splitCache[num]

// Processing rules
let transformStone (stone: int64) : int64 array =
    match stone with
    | 0L -> [| 1L |]
    | _ when hasEvenDigitCount (double stone) ->
        let first, second = splitStone stone
        [| second; first |]
    | _ -> [| stone * 2024L |]

// Apply rules to every element and flatten the array
let transformArray (currentArr: int64 array) : int64 array =
    currentArr |> Array.collect transformStone

let simulateBlinks (initialStones: int64 array) (numBlinks: int) : int64 array =
    let rec applyTransformations acc remainingBlinks =
        if remainingBlinks > 0 then
            let newAcc = transformArray acc
            applyTransformations newAcc (remainingBlinks - 1)
        else
            acc

    applyTransformations initialStones numBlinks

let blink numBlinks =
    let initialStones = organizeInput
    let finalStones = simulateBlinks initialStones numBlinks
    finalStones |> Array.length



//Part 2

let reorganize =
    let input = inputData.ReadAllText 11 "input"

    let frequencySequence =
        input.Split ' ' |> Seq.map int64 |> Seq.map (fun v -> toKeyValue (v, 1L))

    let frequencyCache = new Dictionary<int64, int64>(frequencySequence)

    frequencyCache

let updateDictionary (dict: Dictionary<int64, int64>) (key: int64) (value: int64) =
    match dict.TryGetValue key with
    | true, cachedValue -> dict[key] <- cachedValue + value
    | false, _ -> dict.Add(key, value)

let blinkIterator currentCounts =
    let newCounts = Dictionary<int64, int64>()

    for KeyValue(number, freq) in currentCounts do
        match number with
        | 0L -> updateDictionary newCounts 1 freq
        | _ when double number |> hasEvenDigitCount ->
            pairToList (splitStone number)
            |> List.iter (fun split -> updateDictionary newCounts split freq)
        | _ ->
            let newStone = number * 2024L
            updateDictionary newCounts newStone freq

    newCounts

let blinkIfTheyAreInTheRoomWithYou numBlinks =
    let counterCache = reorganize

    let rec blinkLoop counterCache blinks =
        if blinks <= 0 then
            counterCache
        else
            let newCounts = blinkIterator counterCache

            blinkLoop newCounts (blinks - 1)

    let finalCache = blinkLoop counterCache numBlinks
    finalCache

let partOne = blink 25
printfn "%d" partOne

let partTwo =
    let stonesCache = blinkIfTheyAreInTheRoomWithYou 75
    printfn $"%A{stonesCache |> Seq.toList}\n\n{stonesCache.Values |> Seq.sum} stones in part 2"
