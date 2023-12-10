open Helper

module PipeTraversal =
    type Direction =
    | North
    | South
    | East
    | West
    with
        member this.rev =
            match this with
            | North -> South
            | South -> North
            | East -> West
            | West -> East

    let moveDirection currentLoc direction =
        let (x,y) = currentLoc
        match direction with
        | North -> (x, y - 1)
        | South -> (x, y + 1)
        | East -> (x + 1, y)
        | West -> (x - 1, y)
        
    let pipeToDirMap =
        [|
            "|",[|North;South|] |> Set.ofArray
            "-",[|East;West|] |> Set.ofArray
            "L",[|North;East|] |> Set.ofArray
            "J",[|North;West|] |> Set.ofArray
            "F",[|South;East|] |> Set.ofArray
            "7",[|South;West|] |> Set.ofArray
        |] |> Map.ofArray

    let collectGridAndStartPos (lines: string seq) =
        let mutable startPos = (0,0)
        let grid =
            lines
            |> Seq.mapi(
                fun i line -> 
                    line
                    |> Seq.mapi(fun j c ->
                        if (string c) = "S" then
                            startPos <- (j,i)
                        string c
                    )
                    |> Array.ofSeq
            )
            |> Array.ofSeq
        grid,startPos
    
    let locateValidStartingDirections startingPos (grid: string array array) =
        seq {
            for dir in [|North;South;East;West|] do
                let (x,y) = moveDirection startingPos dir 
                match grid[y][x] with
                | "." -> ()
                | _ ->
                    match pipeToDirMap[grid[y][x]] |> Set.contains dir.rev with
                    | true ->
                        yield dir
                    | false -> ()
        }
    
    let rec traverseLoop allSteps currentLoc enterFromDir (grid: string array array) =
        let (x, y) = currentLoc
        match grid[y][x] with
        | "S" -> allSteps
        | _ as newPipe ->
            let dir = enterFromDir |> Set.singleton |> Set.difference pipeToDirMap[newPipe] |> Set.minElement
            let newLoc = moveDirection currentLoc dir
            traverseLoop (allSteps @ [newLoc]) newLoc dir.rev grid

    let retrieveAllLoopCoords gridAndStartingPos =
        let (grid, startingPos) = gridAndStartingPos
        let startingDir = locateValidStartingDirections startingPos grid |> Seq.head
        let newLoc = moveDirection startingPos startingDir
        traverseLoop [startingPos;newLoc] newLoc startingDir.rev grid

module Part1 =
    open PipeTraversal
    let findFurthestStep gridAndStartingPos =
        let totalSteps = retrieveAllLoopCoords gridAndStartingPos
        (totalSteps |> List.length) / 2

    let total lines =
        collectGridAndStartPos lines
        |> findFurthestStep

module Part2 =
    open PipeTraversal

    let total lines =
        collectGridAndStartPos lines
        |> fst
input
|> outputFileResult Part1.total "Part 1"

//input
//|> outputFileSeq Part2.total "Part 2"
