open Helper

module OasisTraversal =
    let fileConvert lines =
        lines |> Seq.map(fun line -> line |> String.splitOnStringTrim " " |> Array.map(fun num -> num |> System.Int32.Parse))

    

module Part1 =
    open OasisTraversal
    let total lines =
        fileConvert lines
test
|> outputFileSeq Part1.total "Part 1"
