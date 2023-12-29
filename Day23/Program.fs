open Helper

module MazeTraversal =
    open GridNav

    let validDirectionMap = [|'v',South;'>',East|] |> Map.ofArray

    let isValidSlope (sx,sy) (ex,ey) (dir: Direction) (grid: char array2d)  =
        match grid[ex,ey] with
        | '#' -> false
        | 'v'
        | '>' as s -> validDirectionMap[s].rev <> dir
        | _ ->
            match grid[sx,sy] with
            | 'v'
            | '>' as s -> validDirectionMap[s] = dir
            | _ -> true

    let isNotWall (sx, sy) (ex, ey) (dir: Direction) (grid: char array2d) =
        match grid[ex,ey] with
        | '#' -> false
        | _ -> true

    let findAllPaths newDirCheckFunc (grid: char array2d) =
        let startLoc = (1, 0)
        let endLoc = ((grid.GetLength 0) - 2, (grid.GetLength 1) - 1 )

        let rec takeStep pos (totalSteps: int) prevLocs =

            match pos = endLoc with
            | true -> totalSteps
            | false ->
                let pathCounts =
                    Direction.all
                    |> Array.choose(fun dir ->
                        match nextCoordArray grid dir pos with
                        | Some newPos ->
                            match (newDirCheckFunc pos newPos dir grid), (prevLocs |> Set.contains newPos) with
                            | true, false -> Some (newPos)
                            | _, _  -> None
                        | None -> None
                    )
                    |> Array.map(fun newPos -> takeStep newPos (totalSteps + 1) (prevLocs.Add(pos)))
                match pathCounts with
                | [||] -> 0
                | _ -> pathCounts |> Array.max
        
        takeStep startLoc 0 Set.empty

module Part1 =
    open MazeTraversal
    let total lines =
        lines
        |> createArray2D createCharGrid
        |> findAllPaths isValidSlope

module Part2 =
    open MazeTraversal
    let total lines =
        lines
        |> createArray2D createCharGrid
        |> findAllPaths isNotWall

//input
//|> outputFileResult Part1.total "Part 1"

test
|> outputFileResult Part2.total "Part 2"
