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
    
    let createGraph startLoc endLoc (grid: char array2d) =
        let mutable graph: Map<(int*int),Map<(int*int),int>> = Map.empty
        let queue = System.Collections.Generic.Queue<_*_*_*_>()

        let rec traverseGrid() =
            match queue.TryDequeue() with
            | true, (pos, (prevDir: Direction), prevNode, stepsSinceLastNode) ->
                match pos = endLoc with
                | true -> 
                    let prevSet = match graph |> Map.tryFind prevNode with Some s -> s | None -> Map.empty
                    graph <- graph.Add(prevNode, prevSet.Add(pos, stepsSinceLastNode))
                | false ->
                    let validDirections =
                        Direction.all
                        |> Array.filter(fun dir -> dir <> prevDir.rev)
                        |> Array.choose(fun dir -> 
                            match nextCoordArray grid dir pos with
                            | Some newPos when isNotWall pos newPos dir grid ->
                                Some(dir, newPos)
                            | _ -> None
                        )
                    match validDirections |> Array.length with
                    | 0 -> ()
                    | 1 -> 
                        let (newDir, newPos) = validDirections |> Array.head
                        queue.Enqueue(newPos, newDir, prevNode, (stepsSinceLastNode + 1))
                    | _ ->
                        let prevNodeSet = match graph |> Map.tryFind prevNode with Some s -> s | None -> Map.empty
                        graph <- graph.Add(prevNode,prevNodeSet.Add(pos,stepsSinceLastNode))
                        match graph |> Map.containsKey pos with
                        | true -> ()
                        | false -> 
                            graph <- graph.Add(pos, (Map.empty.Add(prevNode, stepsSinceLastNode)))
                            validDirections
                            |> Array.iter(fun (newDir, newPos) -> queue.Enqueue(newPos, newDir, pos, 1))

                traverseGrid()
            | false, _ -> ()
        queue.Enqueue(startLoc, South, startLoc, 0)
        traverseGrid()
        graph

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

    let findAllPathsViaGraph (grid: char array2d) =
        let startLoc = (1, 0)
        let endLoc = ((grid.GetLength 0) - 2, (grid.GetLength 1) - 1 )
        let graph = createGraph startLoc endLoc grid

        let rec takeStep (totalSteps: int) currentNode (prevNodes: Set<_>) =
            match currentNode = endLoc with
            | true -> totalSteps
            | false ->
                let steps = 
                    graph[currentNode]
                    |> Map.filter(fun newLoc _ -> prevNodes |> Set.contains newLoc |> not)
                    |> Map.map(fun newLoc steps -> takeStep (totalSteps + steps) newLoc (prevNodes.Add(currentNode)))
                    |> Map.values
                match steps |> Seq.length with | 0 -> 0 | _ -> steps |> Seq.max

        takeStep 0 startLoc Set.empty

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
        |> findAllPathsViaGraph

input
|> outputFileResult Part1.total "Part 1"

input
|> outputFileResult Part2.total "Part 2"
