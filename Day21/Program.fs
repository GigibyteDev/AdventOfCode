open Helper

module StepCount =
    open GridNav
    open System.Collections.Generic
    let beginSteps totalSteps (grid: char array array) =
        let queue = PriorityQueue<int*int, int>()
        let mutable history: Set<int*int> = Set.empty

        let rec findSteps (stepCache: Map<int*int,int>) =
            match queue.TryDequeue() with
            | false, _, _ -> stepCache
            | true, pos, stepsFromCenter ->
                [|North; South; East; West|]
                |> Array.iter(fun dir ->
                    match (nextCoord grid dir pos) with
                    | Some nPos -> 
                        match grid |> at nPos with
                        | '#' -> ()
                        | _ -> 
                            match history.Contains nPos with
                            | true -> ()
                            | false -> 
                                history <- history.Add nPos
                                queue.Enqueue(nPos, stepsFromCenter + 1)
                    | None -> ()
                )
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
        |> beginSteps 64

input
|> outputFileResult Part1.total "Part 1"
