module Program

open System
open AdventUtilities
open System.Text.RegularExpressions

let inputData = InputData()

let input =
    let inputString = (inputData.ReadAllText 9 "input").Replace("\r\n", "")

    Regex.Split(inputString, @"(?<num>\d)", RegexOptions.ExplicitCapture)
    |> Array.filter (fun s -> s.Length = 1)
    |> Array.map (fun s -> if Char.IsDigit(s[0]) then int (s[0] - '0') else -5)

type Block =
    | Open
    | Written of Id: int

let disk =
    input
    |> Array.indexed
    |> Array.collect (fun (i, n) ->
        let block = if Int32.IsEvenInteger i then Written(i / 2) else Open
        Array.replicate n block)

let checksum (hardDisk: Block array) =
    hardDisk
    |> Array.indexed
    |> Array.sumBy (fun (i, s) ->
        match s with
        | Open -> 0L
        | Written Id -> int64 (i * Id))

let inline swapArray (arr: Block array) i j =
    let temp = arr[i]
    arr[i] <- arr[j]
    arr[j] <- temp
    arr

let compactDisk (hardDisk: Block array) =
    let rec loop left right =
        if left >= right then
            hardDisk
        else
            match hardDisk[left], hardDisk[right] with
            | Open, Open -> loop left (right - 1)
            | Written _, Open -> loop (left + 1) (right - 1)
            | Written _, Written _ -> loop (left + 1) right
            | Open, Written Id ->
                swapArray hardDisk left right |> ignore

                loop (left + 1) (right - 1)

    loop 0 (hardDisk.Length - 1)

let resultOne = disk |> compactDisk |> checksum |> sprintf "%d"



type Segment =
    { Id: int64
      Start: int
      End: int
      Length: int }

let rawData =
    (inputData.ReadAllText 9 "input").Replace("\r\n", "")
    |> Seq.map id
    |> Array.ofSeq
    |> Array.map (Char.GetNumericValue >> int64)

let hdd =
    rawData
    |> Array.indexed
    |> Array.collect (fun (i, n) ->
        let segment = if Int32.IsEvenInteger i then int64 i / 2L else -5L
        Array.replicate (int n) segment)

let findRanges (input: int64 array) =
    let rec loop current ranges start currentElem =
        if current >= input.Length then
            List.rev ((currentElem, start, current - 1) :: ranges)
        else if input[current] = currentElem then
            loop (current + 1) ranges start currentElem
        else
            loop (current + 1) ((currentElem, start, current - 1) :: ranges) current input[current]

    loop 1 [] 0 input[0]

let calculateChecksum (hdd: int64 array) =
    hdd
    |> Array.indexed
    |> Array.sumBy (fun (i, v) -> if v > 0 then int64 i * v else 0L)

let moveFile (freeSpace: Segment) (file: Segment) (length: int64) =
    Array.fill hdd file.Start (int length) 0L
    Array.fill hdd freeSpace.Start (int length) file.Id

let defragment (ranges: (int64 * int * int) list) =
    let freeSegments, fileSegments =
        ranges |> List.partition (fun (num, _, _) -> num = -5L)

    let freeSpaces =
        freeSegments
        |> List.map (fun (_, f, l) ->
            { Id = 0L
              Start = f
              End = l
              Length = l - f + 1 })

    let files =
        fileSegments
        |> List.rev
        |> List.map (fun (id, f, l) ->
            { Id = id
              Start = f
              End = l
              Length = l - f + 1 })

    let rec loopFiles files freeSpaces =
        match files with
        | [] -> ()
        | file :: rest ->
            let fileLength = file.Length

            match
                freeSpaces
                |> List.tryFindIndex (fun space -> space.Length >= fileLength && space.End < file.Start)
            with
            | Some index ->
                let freeSpace = freeSpaces[index]
                moveFile freeSpace file fileLength

                let newSpace =
                    { freeSpace with
                        Start = freeSpace.Start + fileLength
                        Length = freeSpace.Length - fileLength }

                match newSpace.Length with
                | 0 ->
                    let newSpaces = freeSpaces |> List.removeAt index
                    loopFiles rest newSpaces
                | _ ->
                    let newSpaces = freeSpaces |> List.updateAt index newSpace
                    loopFiles rest newSpaces

            | None -> loopFiles rest freeSpaces

    loopFiles files freeSpaces
    hdd

let resultTwo = hdd |> findRanges |> defragment |> calculateChecksum

let partOne = printfn $"The checksum in part one is {resultOne}"
let partTwo = printfn $"The checksum in part two is {resultTwo}"
