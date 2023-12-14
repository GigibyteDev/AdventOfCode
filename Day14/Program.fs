open Helper

module RockPanel =
    type Dir =
    | North
    | South
    | East
    | West
    with 
        member this.startingCoords (grid: char array array) =
            match this with
            | North -> (0,0)
            | South -> (0, grid.Length - 1)
            | East -> (grid[0].Length - 1, 0)
            | West -> (0, 0)

        member this.nextCoord coord (grid: char array array) =
            let (x,y) = coord
            match this with
            | North -> 
                if x + 1 >= (grid[y].Length) then (0, y + 1)
                else (x + 1, y)
            | South ->
                if x + 1 >= (grid[y].Length) then (0, y - 1)
                else (x + 1, y)
            | East ->
                if y + 1 >= (grid.Length) then (x - 1, 0)
                else (x, y + 1)
            | West ->
                if y + 1 >= (grid.Length) then (x + 1, 0)
                else (x, y + 1)

    let createGrid lines =
        lines
        |> Seq.map
            (fun (line:string) ->
                line.ToCharArray()
            )
        |> Array.ofSeq

    let updatePlaceInRow x replaceWith (row: char array) =
        row |> Array.removeAt x |> Array.insertAt x replaceWith

    let replaceRow y func (grid: char array array) =
        let newRow = func grid[y]
        grid |> Array.removeAt y |> Array.insertAt y newRow

    let rec dropRock (grid: char array array) currentRockLoc dropping direction =
        let (x, y) = currentRockLoc
        let rowToReplace, placeInRow, nextCoords =
            match direction with 
            | North -> y + 1, x, (x, y - 1) 
            | South -> y - 1, x, (x, y + 1)
            | East -> y, x - 1, (x + 1, y)
            | West -> y, x + 1, (x - 1, y)

        if (y < 0 || y = grid.Length || x < 0 || x = grid[y].Length) then
            match dropping with
            | true ->
                grid |> replaceRow rowToReplace (updatePlaceInRow placeInRow 'O')
            | false ->
                grid
        else
            match grid[y][x], dropping with
            | '.', true -> dropRock grid nextCoords dropping direction
            | '#', true
            | 'O', true -> grid |> replaceRow rowToReplace (updatePlaceInRow placeInRow 'O')
            | 'O', false -> dropRock (grid |> replaceRow y (updatePlaceInRow x '.')) nextCoords true direction
            | '.', false 
            | '#', false
            | _ -> grid

    let rec tiltDirection' (currentLoc: (int*int)) (direction: Dir) (grid: char array array) =
        let (x,y) = currentLoc
        if y >= (grid.Length) || y < 0 || x >= (grid[y].Length) || x < 0 then
            grid
        else
            let newCoords = direction.nextCoord (x,y) grid
            let newGrid = (dropRock grid (x,y) false direction)
            newGrid |> tiltDirection' newCoords direction
    
    let tiltDirection (direction: Dir) (grid: char array array) =
        let currentLoc = direction.startingCoords grid
        let newGrid = tiltDirection' currentLoc direction grid
        newGrid
    
    let tiltDirectionCheckCache (direction: Dir) gridAndCache =
        let (grid, (cache, timesCacheChecked)) = gridAndCache
        match cache |> Map.tryFind (grid,direction) with
        | Some c -> c, (cache,timesCacheChecked + 1)
        | None ->
            let currentLoc = direction.startingCoords grid
            let newGrid = tiltDirection' currentLoc direction grid
            newGrid, (cache.Add((grid,direction), newGrid), 0)
        
    let rec cycleRocks timesToCycle gridAndCache =
        let (grid, (cache, timesCacheChecked)) = gridAndCache
        if timesToCycle <= 0 || timesCacheChecked >= 4 then grid
        else 
            gridAndCache
            |> tiltDirectionCheckCache North
            |> tiltDirectionCheckCache West
            |> tiltDirectionCheckCache South
            |> tiltDirectionCheckCache East
            |> cycleRocks (timesToCycle - 1)

    let beginCycleRocks timesToCycle grid = cycleRocks timesToCycle (grid, (Map.empty,0))

    let calculateSumOfGrid (grid: char array array) =
        grid
        |> Array.indexed
        |> Array.fold(fun runningTotal (i, row) ->
            let totalRocks = row |> Array.filter(fun c -> c = 'O') |> Array.length
            runningTotal + int64 (totalRocks * (grid.Length - i))
        ) 0L

module Part1 =
    open RockPanel
    let total lines =
        lines 
        |> createGrid
        |> tiltDirection North
        |> calculateSumOfGrid

module Part2 =
    open RockPanel
    let total lines =
        lines
        |> createGrid
        |> beginCycleRocks 1000000000
        |> calculateSumOfGrid

input
|> outputFileResult Part1.total "Part 1"

input
|> outputFileResult Part2.total "Part 2"
