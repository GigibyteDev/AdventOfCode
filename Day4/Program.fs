// For more information see https://aka.ms/fsharp-console-apps
open Helper

module ScratchTicket =
    let getNumsFromString numStr =
        numStr
        |> String.splitOnStringTrim(" ")
        |> Array.choose(fun value -> match value |> System.Int32.TryParse with | true, num -> Some num | _ -> None)

    let getSetFromString numStr =
        numStr
        |> getNumsFromString
        |> Set.ofArray
    
    let getWinnersFromLine line =
        let sets =
            line
            |> String.splitOnStringsTrim [|":";"|"|]
            |> Array.tail
            |> Array.map(fun vals -> getSetFromString vals)
        
        sets
        |> Seq.head
        |> Set.intersect (sets |> Seq.last)

module Part1 =
    open ScratchTicket

    let calcPoints winners =
        ((winners |> Seq.length) - 1)
        |> pown 2

    let total lines =
        lines
        |> Seq.fold(fun total line ->
            let winningVals =
                line
                |> getWinnersFromLine
                |> calcPoints

            total + winningVals
        ) 0

module Part2 =
    open ScratchTicket

    let addAmtToMapAtLoc loc cardCount (gameTallys: Map<int,int>) =
        let value =
            match gameTallys |> Map.tryFind loc with
            | Some amt -> amt + cardCount
            | None -> 1 + cardCount
        gameTallys.Add(loc, value)

    let rec addTallyToNext wins (gameTallys: Map<int, int>) currentLoc cardAmount =
        match wins with
        | 0 ->
            gameTallys
        | _ ->
            addTallyToNext (wins - 1) (gameTallys |> addAmtToMapAtLoc (currentLoc + wins) cardAmount) currentLoc cardAmount

    let rec addGamesFromWins (gameTallys: Map<int, int>) iter (gamesAndWins: Map<int,int>) =
        match gamesAndWins |> Map.tryFind iter with
        | Some wins ->
            let newTally =
                addTallyToNext wins gameTallys iter gameTallys[iter]
            addGamesFromWins newTally (iter + 1) gamesAndWins
        | None ->
            gameTallys

    let beginCounting (gamesAndWins: Map<int,int>) =
        addGamesFromWins (gamesAndWins |> Map.map(fun _ _ -> 1)) 0 gamesAndWins

    let total lines =
        lines
        |> Seq.mapi(fun i line -> 
            i,line 
            |> getWinnersFromLine
            |> Seq.length
        )
        |> Map.ofSeq
        |> beginCounting
        |> Seq.sumBy(fun v -> v.Value)

input
|> outputFileResult Part1.total "Part 1"

input
|> outputFileResult Part2.total "Part 2"
