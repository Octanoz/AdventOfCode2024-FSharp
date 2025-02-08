module Program

open System.IO
open AdventUtilities

let inputData = InputData()
let input = inputData.ReadAllLines 5 "input"

let splitIndex = input |> Array.findIndex (fun s -> s = "")
let rulesSection = input[.. splitIndex - 1]
let manualsSection = input.[splitIndex + 1 ..]

let rules =
    [ for rule in (rulesSection: string array) ->
          let [| leading; trailing |] = rule.Split '|'
          (int leading, int trailing) ]

let manuals =
    manualsSection
    |> Array.map (fun line -> line.Split ',' |> Array.map int |> List.ofArray)
    |> List.ofArray

//Store the index of each page in a map, then check if the rules order corresponds with the index values
let isValid manual (rules: (int * int) list) =
    let pageMap =
        manual
        |> List.indexed
        |> List.map (fun (idx, elem) -> (elem, idx))
        |> Map.ofList

    rules |> List.forall (fun (a, b) -> pageMap[a] < pageMap[b])

//For each manual filter the relevant rules and confirm the order is valid then take the middle page
let checkManuals (manuals: int list list) rules =
    [ for manual in manuals do
          let middle = manual.Length / 2

          let relevantRules =
              rules
              |> List.filter (fun (leading, trailing) ->
                  manual |> List.contains leading && manual |> List.contains trailing)

          if (isValid manual relevantRules) then
              yield manual[middle] ]

let generateManual rules =
    //Transposing the tuples list to two separate lists
    let [ firstRules; secondRules ] =
        rules |> List.map (fun (a, b) -> [ a; b ]) |> List.transpose

    //The countBy function generates a tuple (elem, frequency) so the frequency of the
    //second list elements shows the page positions when ordered by frequency
    let correctedManual =
        secondRules |> List.countBy id |> List.sortBy snd |> List.map fst

    //The first page will only be present in the first rules
    //so take the element from here that isn't present in the second list
    let manualHead = firstRules |> List.except secondRules |> List.head

    manualHead :: correctedManual

//Basically the inverse of the checkManuals function. Still collecting the relevant rules
//but now skip the valid manuals and if it is invalid generate the correct one and take its middle page
let correctInvalidManuals (manuals: int list list) rules =
    [ for manual in manuals do
          let middle = manual.Length / 2

          let relevantRules =
              rules
              |> List.filter (fun (leading, trailing) ->
                  manual |> List.contains leading && manual |> List.contains trailing)

          if (isValid manual relevantRules) then
              ()
          else
              let correctedManual = generateManual relevantRules
              yield correctedManual[middle] ]

let partOne = checkManuals manuals rules |> List.sum

let partTwo = correctInvalidManuals manuals rules |> List.sum

printfn $"Part 1 result: {partOne}\nPart 2 result: {partTwo}\n"
