namespace AdventUtilities

module Helpers =

    [<AutoOpen>]
    module TupleExt =

        let inline mapPair f (a, b) = (f a, f b)
        let inline pairSum (a, b) = a + b
        let inline movePair (row, col) (dRow, dCol) = (row + dRow, col + dCol)
        let inline factorPair (sx, sy) (a, b) = (a * sx, b * sy)
        let inline equalPair (a1, b1) (a2, b2) = a1 = a2 && b1 = b2
        let inline comparePair (a1, b1) (a2, b2) = compare (a1, b1) (a2, b2)
        let inline manhattanPair (a1, b1) (a2, b2) = abs (a1 - a2) + abs (b1 - b2)

        let inline mapTriple f (a, b, c) = (f a, f b, f c)
        let inline sumTriple (a, b, c) = a + b + c
        let inline equalTriple (a1, b1, c1) (a2, b2, c2) = a1 = a2 && b1 = b2 && c1 = c2
        let inline compareTriple (a1, b1, c1) (a2, b2, c2) = compare (a1, b1, c1) (a2, b2, c2)

    module Grid =

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
