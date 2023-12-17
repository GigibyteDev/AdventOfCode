open Helper

module MirrorMaze =
    open Helper.GridNav

    let convertInputToGrid lines =
        lines
        |> Seq.map(fun (line:string) -> line.ToCharArray())
        |> Array.ofSeq

    let gatherAllStartingPositions (grid: char array array) =
        seq {
            for i in [0 .. grid.Length - 1] do yield ((0, i),East)
            for i in [0 .. (grid[0].Length - 1)] do yield ((i,0),South)
            for i in [0 .. grid[0].Length - 1] do yield ((i, grid.Length - 1),North)
            for i in [0 .. grid.Length - 1] do yield ((grid[0].Length - 1, i),West)
        } |> Array.ofSeq

    let topLeftStartOnly grid = gatherAllStartingPositions grid |> Array.head |> Array.singleton

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

    let beginLoop positionGatherFunc grid =
        let mutable cache': Set<_> = Set.empty

        let rec traverseMaze coord (grid: char array array) (dir: Direction) (energizedCoords: Set<int*int>) =
            match coord with
            | Some coord ->
                match cache' .Contains(coord,dir) with
                | true -> energizedCoords |> Set.union energizedCoords
                | false ->
                    cache' <- cache'.Add((coord,dir))
                    let (x,y) = coord
                    let newEnergizedCoords =
                        mirrorConversionMap dir.rev (grid[y][x])
                        |> Array.map(fun newDir -> 
                            let newCoord = (nextCoordNoLoop grid newDir coord)
                            traverseMaze newCoord grid newDir (Set.empty.Add(coord)))
                        |> Set.unionMany
                    energizedCoords |> Set.union newEnergizedCoords
            | None -> energizedCoords

        let startingPositions = 
            positionGatherFunc grid
        let allSets = 
            startingPositions
            |> Seq.map(fun sp -> 
                match sp |> fst = (3,0) with
                | true -> ()
                | false -> ()
                cache' <- Set.empty
                traverseMaze ((sp |> fst) |> Some) grid (sp |> snd) Set.empty
                )
        allSets
        |> Seq.maxBy(fun set -> set |> Seq.length)
        |> Seq.length
        
module Part1 =
    open MirrorMaze
    let total lines =
        lines
        |> convertInputToGrid
        |> beginLoop topLeftStartOnly

module Part2 =
    open MirrorMaze
    let total lines =
        lines
        |> convertInputToGrid
        |> beginLoop gatherAllStartingPositions


input
|> outputFileResult Part1.total "Part 1"

input
|> outputFileResult Part2.total "Part 2"
