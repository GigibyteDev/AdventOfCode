open Helper

module StepCount =
    open GridNav
    open System.Collections.Generic
    
    let retrieveLoopedCoord (grid: 'T array array) (dir: Direction) pos =
        let (nx,ny) = dir.coordChange pos
        let rx =
            match nx with
            | x when x < 0 -> System.Int32.Clamp(grid[0].Length - ((-x) % grid[0].Length), 0, grid[0].Length - 1)
            | x -> (x % grid[0].Length)
        let ry =
            match ny with
            | y when y < 0 -> System.Int32.Clamp(grid.Length - ((-y) % grid.Length), 0, grid.Length - 1)
            | y -> (y % grid.Length)

        Some((nx,ny), grid |> at (rx,ry))

    let retrieveNonLoopedCoord (grid: 'T array array) (dir: Direction) pos =
        let v = nextCoord grid dir pos
        match v with
        | Some nc ->
            Some (nc, grid |> at nc)
        | None -> None

    let beginSteps totalSteps nextCoordFunc (grid: char array array) =
        let queue = PriorityQueue<int*int, int>()
        let mutable history: Set<int*int> = Set.empty

        let rec findSteps (stepCache: Map<int*int,int>) =
            match queue.TryDequeue() with
            | false, _, _ -> stepCache
            | true, pos, stepsFromCenter ->
                match stepsFromCenter <= totalSteps with
                | true ->
                    [|North; South; East; West|]
                    |> Array.iter(fun dir ->
                        match (nextCoordFunc grid dir pos) with
                        | Some (nPos, gridV) -> 
                            match gridV with
                            | '#' -> ()
                            | _ -> 
                                match history.Contains nPos with
                                | true -> ()
                                | false -> 
                                    history <- history.Add nPos
                                    queue.Enqueue(nPos, stepsFromCenter + 1)
                        | None -> ()
                    )
                | false -> ()
                findSteps (stepCache.Add(pos, stepsFromCenter))

        queue.Enqueue((grid[0].Length / 2, grid.Length / 2), 0)

        findSteps Map.empty
        |> Map.filter(fun key v -> v <= totalSteps && v % 2 = 0)
        |> Seq.length

module Part1 =
    open StepCount
    let total lines =
        lines
        |> createCharGrid
        |> beginSteps 64 retrieveNonLoopedCoord

module Part2 =
    open StepCount
    let total lines =
        lines
        |> createCharGrid
        |> beginSteps 500 retrieveLoopedCoord
           

input
|> outputFileResult Part1.total "Part 1"

test
|> outputFileResult Part2.total "Part 2"
