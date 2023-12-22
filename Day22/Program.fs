open Helper

module Bricks =
    type Brick = {
        id: int
        blocks: Set<int*int*int>
        restingBlocks: Set<int>
        restingOn: Set<int>
    }
    with
        static member init (id: int) (start: int*int*int) (fin: int*int*int) =
            let blocks =
                match start, fin with
                | (x1,y1,z1), (x2,y2,z2) when x1 = x2 && y1 = y2 -> seq { for z in z1..z2 do yield (x1, y1, z) } |> Set.ofSeq
                | (x1,y1,z1), (x2,y2,z2) when x1 = x2 && z1 = z2 -> seq { for y in y1..y2 do yield (x1, y, z1) } |> Set.ofSeq
                | (x1,y1,z1), (x2,y2,z2) when y1 = y2 && z1 = z2 -> seq { for x in x1..x2 do yield (x, y1, z1) } |> Set.ofSeq
                | _ -> start |> Set.singleton
            {id = id; blocks = blocks; restingBlocks = Set.empty; restingOn = Set.empty}

    let createBricks lines =
        lines
        |> Seq.map(fun (line:string) -> 
            line 
            |> String.splitOnStringTrim "~"
            |> Array.map(fun cs -> 
                let coords =
                    cs 
                    |> String.splitOnStringTrim "," 
                    |> Array.map(fun v -> System.Int32.Parse v)
                coords[0],coords[1],coords[2]
            )
        )
        |> Seq.sortBy(fun coords -> coords |> Array.map(fun (_,_,z) -> z) |> Array.min)
        |> Seq.mapi(fun i coords -> (i+1), Brick.init (i+1) coords[0] coords[1])
        |> Map.ofSeq 

    let rec findBrickBelowAndUpdateMap (allBricks: Map<int,Brick>) brickToDrop =
        let newBrickLocs = brickToDrop.blocks |> Set.map(fun (x,y,z) -> (x,y,z - 1))
        if newBrickLocs |> Set.exists(fun (_,_,z) -> z <= 0) then allBricks.Add(brickToDrop.id, brickToDrop)
        else
            let bricksBelow = 
                allBricks
                |> Map.filter(fun key brickToCheckAgainst -> key < brickToDrop.id && (brickToCheckAgainst.blocks |> Set.intersect newBrickLocs) |> Seq.length > 0)
            match bricksBelow |> Seq.length > 0 with
            | true ->
                let newMap =
                    bricksBelow |> Map.fold(fun (ab: Map<int,Brick>) _ bb ->
                        ab.Add(bb.id, {bb with restingBlocks = bb.restingBlocks.Add(brickToDrop.id)})
                    ) allBricks
                newMap.Add(brickToDrop.id, {brickToDrop with restingOn = (brickToDrop.restingOn |> Set.union (bricksBelow |> Seq.map(fun bb -> bb.Key) |> Set.ofSeq))})
            | false ->
                findBrickBelowAndUpdateMap allBricks {brickToDrop with blocks = newBrickLocs}

    let findSupportBricks (initBricks: Map<int,Brick>) =
        let mutable bricks = initBricks

        let rec dropBrick brickId =
            match bricks |> Map.tryFind brickId with
            | Some brick ->
                bricks <- findBrickBelowAndUpdateMap bricks brick
                dropBrick (brickId + 1)
            | None -> ()

        dropBrick 1
        bricks 

    let countDisintegratableBricks bricks =
        bricks 
        |> Map.filter(fun key b -> bricks |> Map.exists(fun _ v -> v.restingOn.Contains(key) && v.restingOn |> Seq.length = 1) |> not)
        |> Array.ofSeq
        |> Seq.length

module Part1 =
    open Bricks
    let total lines =
        lines
        |> createBricks 
        |> findSupportBricks
        |> countDisintegratableBricks
input
|> outputFileResult Part1.total "Part 1"