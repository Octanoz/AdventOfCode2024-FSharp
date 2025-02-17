namespace AdventUtilities

module Helpers =

    [<AutoOpen>]
    module TupleExt =

        let inline twin x = x, x
        let inline mapPair f (a, b) = f a, f b
        let inline mapFst f (a, b) = f a, b
        let inline mapSnd f (a, b) = a, f b
        let inline apply f (a, b) = f a b
        let inline pairToList (a, b) = [ a; b ]

        let inline toKeyValue (k, v) =
            System.Collections.Generic.KeyValuePair(k, v)

        let inline fromKeyValue (KeyValue(k, v)) = k, v
        let inline pairSum (a, b) = a + b
        let inline swap (a, b) = b, a
        let inline movePair (row, col) (dRow, dCol) = row + dRow, col + dCol
        let inline diffPair (row, col) (otherRow, otherCol) = row - otherRow, col - otherCol
        let inline factorPair (sx, sy) (a, b) = a * sx, b * sy
        let inline multiply2 (a, b) : int64 = a * b
        let inline equalPair (a1, b1) (a2, b2) = a1 = a2 && b1 = b2
        let inline comparePair (a1, b1) (a2, b2) = compare (a1, b1) (a2, b2)
        let inline manhattanPair (a1, b1) (a2, b2) = abs (a1 - a2) + abs (b1 - b2)

        let inline triplet x = x, x, x
        let inline mapTriple f (a, b, c) = f a, f b, f c
        let inline apply3 f (a, b, c) = f a b c
        let inline tripleToList (a, b, c) = [ a; b; c ]
        let inline pairWise3 (a, b, c) = (a, b), (b, c)
        let inline sum3 (a, b, c) = a + b + c
        let inline equal3 (a1, b1, c1) (a2, b2, c2) = a1 = a2 && b1 = b2 && c1 = c2
        let inline compare3 (a1, b1, c1) (a2, b2, c2) = compare (a1, b1, c1) (a2, b2, c2)

    module StringEx =
        let inline replace (target: string) (replacement: string) (source: string) = source.Replace(target, replacement)

        let inline isDigit (c: char) = System.Char.IsDigit c

        let inline getNums (separator: char) (line: string) =
            line.Split(separator)
            |> Array.where (fun s -> s |> String.forall (fun c -> isDigit c))
            |> Array.map int
            |> List.ofArray

        let inline splitClean (separator: char) (line: string) : string list =
            line.Split(separator, System.StringSplitOptions.RemoveEmptyEntries)
            |> List.ofArray

        let inline keepSplitter (separator: string) (line: string) : string list =
            let pattern = $@"(?<splitter>{separator})"

            System.Text.RegularExpressions.Regex.Split(
                line,
                pattern,
                System.Text.RegularExpressions.RegexOptions.ExplicitCapture
            )
            |> List.ofArray

    module Grid =
        let stringArrayTo2DCharArray (input: string array) =
            let maxRow = input.Length
            let maxCol = input[0].Length
            Array2D.init maxRow maxCol (fun i j -> if j < maxCol then input[i][j] else ' ')

        type Direction =
            | Up
            | Right
            | Down
            | Left

        type Position = int * int

        let changeDirection =
            function
            | Up -> Right
            | Right -> Down
            | Down -> Left
            | Left -> Up

        let move num =
            [ (-1, -1); (-1, 0); (-1, 1); (0, -1); (0, 1); (1, -1); (1, 0); (1, 1) ][num]

        //up right down left
        let dof4 =
            seq {
                move 1
                move 4
                move 6
                move 3
            }

        let isWithinLimits (row, col) (grid: char[,]) =
            let maxRow = grid |> Array2D.length1
            let maxCol = grid |> Array2D.length2
            row >= 0 && row < maxRow && col >= 0 && col < maxCol

        let isObstacle (row, col) (grid: char[,]) =
            isWithinLimits (row, col) grid && grid[row, col] = '#'

        let anyFourNeighbours (row, col) =
            [ for dRow, dCol in dof4 -> movePair (row, col) (dRow, dCol) ]

        let checkNeighbours (row, col) grid =
            [ for moveRow, moveCol in dof4 -> movePair (moveRow, moveCol) (row, col) ]
            |> List.filter (fun (a, b) -> isWithinLimits (a, b) grid)

        let dotNeighbours2D grid (row, col) =
            dof4
            |> Seq.filter (fun (dRow, dCol) ->
                let newRow, newCol = movePair (dRow, dCol) (row, col)
                isWithinLimits (newRow, newCol) grid && grid[newRow, newCol] = '.')
            |> List.ofSeq

        let dotNeighboursJagged grid (row, col) =
            let maxRow = grid |> Array.length
            let maxCol = grid[0] |> Array.length

            let positionWithinLimits row col =
                row >= 0 && row < maxRow && col >= 0 && col < maxCol

            dof4
            |> Seq.filter (fun (dRow, dCol) ->
                let newRow, newCol = movePair (dRow, dCol) (row, col)
                positionWithinLimits newRow newCol && grid[newRow][newCol] = '.')
            |> List.ofSeq

        let isBlocked currentPosition dir grid =
            match dir with
            | Up -> grid |> isObstacle (movePair currentPosition (move 1))
            | Right -> grid |> isObstacle (movePair currentPosition (move 4))
            | Down -> grid |> isObstacle (movePair currentPosition (move 6))
            | Left -> grid |> isObstacle (movePair currentPosition (move 3))

    module Memoization =
        open System.Collections.Generic
        open System.Collections.Concurrent
        open System.Threading

        let inline memoizeInner (cache: Dictionary<_, _>) f x =
            match cache.TryGetValue x with
            | true, v -> v
            | _ ->
                let result = f x
                cache.Add(x, result)
                result

        // Wrap f x into a cached version backed by a thread local Dictionary. Caches aren't shared between threads.
        let memoizeThreadLocal f =
            let cacheTl = new ThreadLocal<Dictionary<_, _>>(fun () -> Dictionary())

            fun x ->
                let cache = cacheTl.Value
                memoizeInner cache f x

        /// Wrap f x into a cached version backed by a ConcurrentDictionary. Caches shared between threads.
        let memoizeConcurrent (f: 'a -> 'b) =
            let cache = ConcurrentDictionary<'a, 'b>()
            fun x -> cache.GetOrAdd(x, f)
