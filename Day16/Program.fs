open Helper

module MirrorMaze =
    open Helper.GridNav

    let convertInputToGrid lines =
        lines
        |> Seq.map(fun (line:string) -> line.ToCharArray())
        |> Array.ofSeq

    let mirrorConversionMap (enterDir: Direction) mirrorType =
        match mirrorType with
        | '|' -> 
            match enterDir with
            | North
            | South -> [|enterDir.rev|]
            | _ -> [|North;South|]
        | '-' ->
            match enterDir with
            | East
            | West -> [|enterDir.rev|]
            | _ -> [|East;West|]
        | '/' -> 
            match enterDir with
            | North -> [|West|]
            | West -> [|North|]
            | South -> [|East|]
            | East -> [|South|]
        | '\\' -> 
            match enterDir with
            | North -> [|East|]
            | East -> [|North|]
            | South -> [|West|]
            | West -> [|South|]
        | _ -> [|enterDir.rev|]

    let beginLoop grid =
        let mutable cache: Set<_> = Set.empty
        let rec traverseMaze coord (grid: char array array) (dir: Direction) (energizedCoords: Set<(int*int)>) =
            match coord with
            | Some coord ->
                match cache.Contains (coord, dir) with
                | true -> energizedCoords
                | false ->
                    cache <- cache.Add(coord, dir)
                    let (x, y) = coord
                    mirrorConversionMap dir.rev (grid[y][x])
                    |> Array.map(fun newDir -> traverseMaze (nextCoordNoLoop grid newDir coord) grid newDir (energizedCoords.Add(coord)))
                    |> Set.unionMany
            | None -> energizedCoords

        traverseMaze ((0,0) |> Some) grid East Set.empty
module Part1 =
    open MirrorMaze
    let total lines =
        lines
        |> convertInputToGrid
        |> beginLoop

input
|> outputFileSeq Part1.total "Part 1"
