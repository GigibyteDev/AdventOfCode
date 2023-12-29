open Helper

module StepCount =
    open GridNav
    open System.Collections.Generic

    let retrieveNonLoopedCoord (grid: 'T array array) (dir: Direction) pos =
        let v = nextCoord grid dir pos
        match v with
        | Some nc ->
            Some (nc, grid |> at nc)
        | None -> None
    
    let countSteps (totalSteps,steps) =
        steps
        |> Map.filter(fun key v -> v <= totalSteps && v % 2 = 0)
        |> Seq.length

    let diamondCount (_, (steps: Map<int*int,int>)) =
        // Algorithm From https://github.com/villuna/aoc23/wiki/A-Geometric-solution-to-advent-of-code-2023,-day-21
        let evens = steps |> Map.filter(fun key v -> (v % 2) = 0 && v > 65) |> Seq.length |> int64
        let odds = steps |> Map.filter(fun key v -> (v % 2) <> 0 && v > 65) |> Seq.length |> int64

        let evenFull = steps |> Map.filter(fun k v -> (v % 2) = 0) |> Seq.length |> int64
        let oddFull = steps |> Map.filter(fun k v -> (v % 2) <> 0) |> Seq.length  |> int64

        let n = 202300L

        let p2 = ((n + 1L)*(n + 1L)) * oddFull + (n*n) * evenFull - (n + 1L) * odds + n * evens
        p2

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

        totalSteps, findSteps Map.empty

module Part1 =
    open StepCount
    let total lines =
        lines
        |> createCharGrid
        |> beginSteps 64 retrieveNonLoopedCoord
        |> countSteps

module Part2 =
    open StepCount
    let total lines =
        lines
        |> createCharGrid
        |> beginSteps 500 retrieveNonLoopedCoord
        |> diamondCount

input
|> outputFileResult Part1.total "Part 1"

input
|> outputFileResult Part2.total "Part 2"
