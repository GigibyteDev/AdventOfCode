open Helper

module DiggyDiggyHole =
    open GridNav
    type DigInstruction = {
        length: int64
        dir: Direction
    }
    let linesToInstructions lines =
        lines |> Seq.map(fun line -> 
            let data = line |> String.splitOnString " "
            {
                length = data[1] |> System.Int64.Parse
                dir = match data[0] with | "U" -> North | "D" -> South | "L" -> West | _ -> East
            }
        )

    let hexLinesToInstructions lines =
        lines |> Seq.map(fun line ->
            let hex = (line |> String.splitOnString " " |> Seq.last).Trim([|'(';')';'#'|])
            let dir = match (hex.Chars (hex.Length - 1)) with | '0' -> East | '1' -> South | '2' -> West | _ -> North
            let hexval = System.Int64.Parse(hex.Substring(0, 5), System.Globalization.NumberStyles.HexNumber)
            {
                length = hexval
                dir = dir
            }
        ) |> Array.ofSeq

    let digHole instructions =
        let (points, _, _, totalPerim) =
            instructions
            |> Seq.fold(fun (lines, curX, curY, totalPerim) ins ->
                let (nCurX, nCurY, (totalPerim: int64)) =
                    match ins.dir with
                    | North -> curX, (curY - ins.length), totalPerim + ins.length
                    | East -> (curX + ins.length), curY, totalPerim + ins.length
                    | South -> curX, (curY + ins.length), totalPerim + ins.length
                    | West -> (curX - ins.length), curY, totalPerim + ins.length
                ((nCurX,nCurY) :: lines, nCurX, nCurY, totalPerim)
            ) (List.empty, 0L, 0L, 0L)
        (points, totalPerim)

    let shoeLace (points: (int64*int64) list) =
        points
        |> List.pairwise 
        |> List.map(fun ((x1,y1), (x2,y2)) -> x1 * y2 - x2 * y1)
        |> List.sum
        |> abs
        |> fun x -> x / 2L

    let picksTheorem (points, perim) =
        let area = (shoeLace points)
        area + perim / 2L + 1L

module Part1 =
    open DiggyDiggyHole
    let total lines =
        lines
        |> linesToInstructions
        |> digHole
        |> picksTheorem

module Part2 =
    open DiggyDiggyHole
    let total lines =
        lines
        |> hexLinesToInstructions
        |> digHole
        |> picksTheorem

input
|> outputFileResult Part1.total "Part 1"

input
|> outputFileResult Part2.total "Part 2"
