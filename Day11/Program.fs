open Helper

module CosmicMap =
    let createCosmicMapAndExpansionRowsAndColumns (lines: string seq) =
        let allGridColsAndRows =
            (lines |> Seq.mapi(fun i _ -> (0,i)) |> Seq.append (lines |> Seq.head |> Seq.mapi(fun i _ -> (i,0)))) |> Set.ofSeq
        let mutable gridsWithGalaxies = allGridColsAndRows
        let grid =
            lines
            |> Seq.indexed
            |> Seq.map(fun (y, line) ->
                line
                |> Seq.indexed
                |> Seq.map(fun (x, char) ->
                    match string char with
                    | "#" ->
                        gridsWithGalaxies <- gridsWithGalaxies.Remove((x,0)).Remove((0,y))
                    | _ -> ()
                    string char
                )
                |> Array.ofSeq
            )
            |> Array.ofSeq
        grid, gridsWithGalaxies

module Part1 =
    open CosmicMap
    let total lines =
        let (grid, expansionCoords) = createCosmicMapAndExpansionRowsAndColumns lines
        expansionCoords
                

input
|> outputFileSeq Part1.total "Part 1"