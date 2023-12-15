open Helper

module Test =

    let loopLogic coords (grid: int array array) total =
        let (x, y) = coords
        total + (grid[y][x])

    let loopLogic2 coords (grid: int array array) (prevNum, total) =
        let (x, y) = coords
        if (prevNum % 2 = 0) then
            grid[y][x], total + (grid[y][x])
        else
            grid[y][x], total + 1

    let grid =
        seq {
            for x in [1..200] do
                seq{
                    for y in [1..157] do
                        yield y
                } |> Array.ofSeq
        } |> Array.ofSeq

printfn $"{GridNav.loopThroughGrid2 Test.grid Test.loopLogic2 0 0}"
