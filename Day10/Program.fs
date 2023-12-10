open Helper

module PipeTraversal =
    type Direction =
    | North
    | South
    | East
    | West

    let moveDirection currentLoc direction =
        let (x,y) = currentLoc
        match direction with
        | North -> (x, y + 1)
        | South -> (x, y - 1)
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

module Part1 =
    open PipeTraversal
    let total =
        collectGridAndStartPos

input
|> outputFileSeqAndResult Part1.total "Part 1"
