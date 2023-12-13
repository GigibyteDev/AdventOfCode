open Helper

module Springs =
    let unfoldSpring line =
        let arr = line |> String.splitOnString " "
        let unfoldedMap =
            seq {
                for _ in [1..5] do
                    yield arr[0]
            } 
            |> String.concat "?" 
            |> List.ofSeq
        let unfoldedGroups =
            seq {
                for _ in [1..5] do
                    yield! (arr[1] |> String.splitOnString "," |> Array.map int)
            } |> Array.ofSeq
        unfoldedMap, unfoldedGroups

    let retrieveLineAndGroups line =
        let arr = line |> String.splitOnStrings [ " "; "," ]
        arr |> Array.head |> List.ofSeq, arr |> Array.tail |> Array.map int

    let beginCountingSuccesses parsedLine =
        let (rem, remGroups) = parsedLine
        let mutable (cache:Map<_,int64>) = Map.empty
        let rec countSuccesses rem remGroups currentGroupRem skipNext =
            match cache |> Map.tryFind (rem, remGroups, currentGroupRem, skipNext) with
            | Some res -> res
            | None ->
                // This logic was pulled from user /u/r_so9 from this comment: https://www.reddit.com/r/adventofcode/comments/18ge41g/comment/kd0cmov/?utm_source=share&utm_medium=web2x&context=3
                // I was stumped until I was able to walk through this for about 3 hours straight... Much was learned.
                let result =
                    match rem, remGroups, currentGroupRem, skipNext with
                    | [], [], 0, _ -> 1L
                    | '?' :: tail, nextGroup :: restOfGroups, 0, false -> countSuccesses tail restOfGroups (nextGroup - 1) (nextGroup = 1) + (countSuccesses tail remGroups 0 false)
                    | '?' :: tail, [], 0, false
                    | '?' :: tail, _, _, true
                    | '.' :: tail, _, 0, _ -> countSuccesses tail remGroups 0 false
                    | '#' :: tail, nextGroup :: restOfGroups, 0, false -> countSuccesses tail restOfGroups (nextGroup - 1) (nextGroup = 1)
                    | '?' :: tail, _, _, false
                    | '#' :: tail, _, _, false -> countSuccesses tail remGroups (currentGroupRem - 1) (currentGroupRem = 1)
                    | _ -> 0L
                cache <- cache.Add((rem, remGroups, currentGroupRem, skipNext), result)
                result
        countSuccesses rem (remGroups |> List.ofArray) 0 false

module Part1 =
    open Springs
    let total lines =
        lines
        |> Seq.map(fun line -> line |> retrieveLineAndGroups |> beginCountingSuccesses)
        |> Seq.sum

module Part2 =
    open Springs
    let total lines =
        lines
        |> Seq.map(fun line -> line |> unfoldSpring |> beginCountingSuccesses)
        |> Seq.sum

input
|> outputFileResult Part1.total "Part 1"

input
|> outputFileResult Part2.total "Part 2"
