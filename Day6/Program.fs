open Helper

module Part1 =
    let total lines =
        lines

test
|> readLines 
|> outputFileResultSeq Part1.total "Part 1"
