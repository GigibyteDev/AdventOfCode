open Helper

module Grid =
    let isValidCoord curX curY (data: char array array) =
        (curX < 0 || curY < 0 || curY >= (data |> Seq.length) || curX >= (data[curY] |> Seq.length)) |> not
    
    let isOfChar (charToMatch: string) (char: char) =
        char = (charToMatch.Chars 0)

    let meetsReq curX curY data req =
        match isValidCoord curX curY data with
        | true ->
            data[curY][curX] |> req
        | false -> false

    let isDigit curX curY data =
        fun (charToMatch) ->
            charToMatch |> String.isDigit
        |> meetsReq curX curY data

    let isSpecialChar curX curY data =
        fun (charToMatch: char) ->
            match charToMatch |> string with
            | "." -> false
            | _ as currVal -> 
                currVal.Chars 0 |> String.isDigit |> not
        |> meetsReq curX curY data

    let numFromChars runningNum =
        runningNum |> Seq.map(fun (c:char) -> string c) |> String.concat "" |> System.Int32.Parse

module Part1 =
    open Grid

    let rec checkAboveAndBelowChar curX curY xOff yOff data =
        match isSpecialChar (curX + xOff) (curY + yOff) data with
        | true -> true
        | false ->
            match xOff, yOff with
            | 1, 1 -> false
            | 1, -1 -> checkAboveAndBelowChar curX curY -1 1 data
            | _, _ -> checkAboveAndBelowChar curX curY (xOff + 1) yOff data
    

    let rec retrieveNum curX curY data (runningNum: char list) =
        match isValidCoord curX curY data with
        | true ->
            match data[curY][curX] |> String.isDigit with
            | true ->
                retrieveNum (curX - 1) curY data (data[curY][curX] :: runningNum)
            | false ->
                runningNum |> numFromChars
        | false ->
            runningNum |> numFromChars

    let rec checkFoundNum curX curY data prevValid currentLength =
        let isValid = 
            match prevValid with
            | true -> true
            | false ->
                let isValidBehind =
                    match currentLength with
                    | 1 ->
                        isSpecialChar (curX - 1) curY data
                    | _ -> false
                match isValidBehind with
                | true -> true
                | false ->
                    match isSpecialChar (curX + 1) curY data with
                    | true -> true
                    | false ->
                        checkAboveAndBelowChar curX curY -1 -1 data
        
        match isDigit (curX + 1) curY data with
        | false ->
            match isValid with
            | true ->
                retrieveNum curX curY data List.empty |> Some
            | false ->
                None
        | true ->
            checkFoundNum (curX + 1) curY data isValid (currentLength + 1)
            
                
    let rec mainLoop curX curY runningTotal (data: char array array) =
        let currentDigit = data[curY][curX]
        let newTotal,lengthToGoForward =
            match currentDigit |> String.isDigit with
            | true ->
                match checkFoundNum curX curY data false 1 with
                | Some num ->
                    (num + runningTotal), num |> string |> String.length
                | None -> runningTotal, 1
            | false -> runningTotal, 1

        if curX + lengthToGoForward < (data[curY] |> Array.length) then
            mainLoop (curX + lengthToGoForward) curY newTotal data
        else
            if curY + 1 < (data |> Array.length) then
                mainLoop 0 (curY + 1) newTotal data
            else newTotal
                
    let total() =
        File.readLines "Data\input.txt"
        |> Seq.map(fun l -> l |> Array.ofSeq)
        |> Array.ofSeq
        |> mainLoop 0 0 0

module Part2 = 
    open Grid

    let rec extractPrevNumFromFoundDigit curX curY data runningNums =
        match isDigit (curX - 1) curY data with
        | true ->
            extractPrevNumFromFoundDigit (curX - 1) curY data (data[curY][curX - 1] :: runningNums)
        | false -> 
           runningNums

    let rec extractPostNumFromFoundDigit curX curY data runningNums =
        match isDigit (curX + 1) curY data with
        | true ->
            extractPostNumFromFoundDigit (curX + 1) curY data (List.append runningNums (data[curY][curX + 1] |> List.singleton))
        | false ->
            runningNums

    let extractFullNumFromFoundDigit curX curY data =
        let prevChars = extractPrevNumFromFoundDigit curX curY data List.empty
        let postChars = extractPostNumFromFoundDigit curX curY data List.empty
        let currentChar = data[curY][curX]
        List.append prevChars ( currentChar :: postChars ) |> numFromChars, postChars |> List.length

    let rec checkAboveAndBelow curX curY xOff yOff data numsFound =
        match numsFound |> List.length > 2 with
        | true ->
            numsFound
        | false ->
            let newNumsFound, newXOff =
                match isDigit (curX + xOff) (curY + yOff) data with
                | true ->
                    let (num, moveForw) = extractFullNumFromFoundDigit (curX + xOff) (curY + yOff) data
                    (num :: numsFound), (match (xOff + moveForw) > 1 with | true -> 1 | false -> (xOff + moveForw))
                | false -> 
                    numsFound, xOff
            
            match newXOff, yOff with
            | 1, 1 -> newNumsFound
            | 1, -1 -> checkAboveAndBelow curX curY -1 1 data newNumsFound
            | _, _ -> checkAboveAndBelow curX curY (newXOff + 1) yOff data newNumsFound

    let rec checkFoundGear curX curY data foundNums =
        let allFoundNums =
            seq{
                yield! checkAboveAndBelow curX curY -1 -1 data List.empty
                match isDigit (curX - 1) curY data with
                | true ->
                    let (numFound, _) = extractFullNumFromFoundDigit (curX - 1) curY data
                    yield numFound
                | false -> ()
                match isDigit (curX + 1) curY data with
                | true ->
                    let (numFound, _) = extractFullNumFromFoundDigit (curX + 1) curY data
                    yield numFound
                | false -> ()
            } |> Array.ofSeq
        match allFoundNums |> Array.length = 2 with
        | true ->
            (allFoundNums |> Array.head) * (allFoundNums |> Array.last)
        | false -> 0

    let rec mainLoop curX curY runningTotal (data: char array array) =
        let newTotal =
            match isOfChar "*" |> meetsReq curX curY data with
            | true ->
                runningTotal + (checkFoundGear curX curY data list.Empty)
            | false ->
                runningTotal

        if curX + 1 < (data[curY] |> Array.length) then
            mainLoop (curX + 1) curY newTotal data
        else
            if curY + 1 < (data |> Array.length) then
                mainLoop 0 (curY + 1) newTotal data
            else newTotal

    let total() = 
        File.readLines "Data\input.txt"
        |> Seq.map(fun l -> l |> Array.ofSeq)
        |> Array.ofSeq
        |> mainLoop 0 0 0

Part1.total |> outputResult "Part 1"
Part2.total |> outputResult "Part 2"