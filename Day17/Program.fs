open Helper

module CrucibleMap =
    open GridNav
    open System
    open System.Collections.Generic

    type Node = {
        visited: bool
        distance: int
    }
    with
        static member Default =
            {
                visited = false
                distance = Int32.MaxValue
            }

    let traverseMap (minMoves:int) (maxMoves: int) (start: (int*int)) (final: (int*int)) (grid: int array array)  =
        let (fx, fy) = final
        let (sx, sy) = start
        let dirMap = [|North,0;East,1;South,2;West,3|] |> Map.ofArray
        let dirMapRev = [|0,North;1,East;2,South;3,West|] |> Map.ofArray

        let graph = Array4D.create grid[0].Length grid.Length 4 (maxMoves + 1) Node.Default
        let queue = PriorityQueue<int*int*int*int, int>()

        let isVisited (x, y, dir, moves) = graph[x, y, dir, moves].visited
        let dist (x, y, dir, moves) = graph[x,y,dir,moves].distance
        let minDist x y =
            [ for dir in 0..3 do
                for moves in 0..maxMoves do
                    dist (x, y, dir, moves)
            ] |> List.min

        [|North;South;East;West|]
        |> Array.choose(fun dir -> match nextCoord grid dir start with | Some _ -> Some dirMap[dir] | None -> None)
        |> Array.iter(fun dir ->
            queue.Enqueue((sx,sy,dir,0),0)
            graph[sx, sy, dir, 0] <- {graph[sx, sy, dir, 0] with distance = 0}
        )

        let retrieveNextPossibleMoves x y dir m =
            [|
                if m < maxMoves then
                    yield dir, (m + 1)
                if m >= minMoves then
                    match dir with
                    | 0
                    | 2 -> yield 1, 1; yield 3, 1
                    | _ -> yield 0, 1; yield 2, 1
            |]
            |> Array.choose(fun (ndir, nm) ->
                match nextCoord grid dirMapRev[ndir] (x,y) with
                | Some c ->
                    let (nx, ny) = c
                    Some (nx, ny, ndir, nm)
                | None -> None
            )

        let rec traverseMap' () =
            match queue.Count, queue.Dequeue() with
            | 0, _ -> minDist fx fy
            | _, node when isVisited node -> traverseMap'()
            | _, (x,y,_,_) when (x,y) = final -> minDist x y
            | _, (x,y,dir,moves) ->
                graph[x, y, dir, moves] <- {graph[x, y, dir, moves] with visited = true}
                retrieveNextPossibleMoves x y dir moves
                |> Array.map(fun (nx, ny, ndir, nm) ->
                    ((nx, ny, ndir, nm), dist (x, y, dir, moves) + (grid |> at (nx,ny)))
                )
                |> Array.filter(fun (n,d) -> d < dist n)
                |> Array.iter(fun ((x,y,dir,m), d) -> 
                    graph[x,y,dir,m] <- {graph[x,y,dir,m] with distance = d}
                    queue.Enqueue((x,y,dir,m),d)
                )
                traverseMap'()

        traverseMap'()


module Part1 =
    open CrucibleMap
    let total lines =
        let grid = createIntGrid lines
        grid
        |> traverseMap 0 3 (0,0) (grid[0].Length - 1, grid.Length - 1)

module Part2 =
    open CrucibleMap
    let total lines =
        let grid = createIntGrid lines
        grid
        |> traverseMap 4 10 (0,0) (grid[0].Length - 1, grid.Length - 1)

input
|> outputFileResult Part1.total "Part 1"

input
|> outputFileResult Part2.total "Part 2"
