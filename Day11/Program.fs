open Helper

module CosmicMap =
    let createCosmicMapAndInfo (lines: string seq) =
        let mutable allGridColsAndRows =
            (lines |> Seq.mapi(fun i _ -> (0,i)) |> Seq.append (lines |> Seq.head |> Seq.mapi(fun i _ -> (i,0)))) |> Set.ofSeq
        let mutable galaxyCoords = Set.empty
        let grid =
            lines
            |> Seq.indexed
            |> Seq.map(fun (y, line) ->
                line
                |> Seq.indexed
                |> Seq.map(fun (x, char) ->
                    match string char with
                    | "#" ->
                        allGridColsAndRows <- allGridColsAndRows.Remove((x,0)).Remove((0,y))
                        galaxyCoords <- galaxyCoords.Add(((galaxyCoords |> Seq.length) + 1),(x,y))
                    | _ -> ()
                    string char
                )
                |> Array.ofSeq
            )
            |> Array.ofSeq
        grid, allGridColsAndRows, galaxyCoords

    let findGalaxyDistance (startCoords: (int*int)) endCoords (expansionCoords: Set<int*int>) (expansionAmt: int64) =
        let (startX, startY) = startCoords
        let (endX, endY) = endCoords
        let (xDis: int64) =
            (match startX > endX with | true -> (int64 startX - int64 endX) | false -> (int64 endX - int64 startX)) + (int64 (expansionCoords |> Set.filter(fun (expX, _) -> (expX > startX && expX < endX) || (expX < startX && expX > endX)) |> Seq.length) * (expansionAmt - 1L))
        let (yDis: int64) =
            (match startY > endY with | true -> (int64 startY - int64 endY) | false -> (int64 endY - int64 startY)) + (int64 (expansionCoords |> Set.filter(fun (_, expY) -> (expY > startY && expY < endY) || (expY < startY && expY > endY)) |> Seq.length) * (expansionAmt - 1L))
        xDis + yDis

    let addUpGalaxyDistances expansionCoords galaxyCoords expansionAmt =
        galaxyCoords
        |> Set.fold(fun totalDis' (gId,startCoords) ->
            galaxyCoords
            |> Set.fold(fun totalDis (gId2,endCoords) ->
                match gId2 > gId with
                | true ->
                    totalDis + (findGalaxyDistance startCoords endCoords expansionCoords expansionAmt)
                | false -> totalDis
            ) totalDis'
        ) 0L
module Part1 =
    open CosmicMap

    let total lines =
        let (grid, expansionCoords, galaxyCoords) = createCosmicMapAndInfo lines
        addUpGalaxyDistances expansionCoords galaxyCoords 2L
       
module Part2 =
    open CosmicMap

    let total lines =
        let (grid, expansionCoords, galaxyCoords) = createCosmicMapAndInfo lines
        addUpGalaxyDistances expansionCoords galaxyCoords 1000000L
input
|> outputFileResult Part1.total "Part 1"

input
|> outputFileResult Part2.total "Part 2"