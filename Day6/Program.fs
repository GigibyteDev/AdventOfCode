open Helper

module Boat =
    let splitLine line =
        line
        |> String.splitOnStringsTrim [|":";" "|]
        |> Array.tail
        |> Array.map(fun str -> str |> System.Int64.Parse)

    let collectValuesAsSeperateNums lines =
        lines
        |> Seq.map(fun line ->
            line
            |> splitLine
        )

    let collectValuesAsSingleNum lines =
        lines
        |> Seq.map(fun line ->
            line
            |> String.replace " " ""
            |> splitLine
        )

    let collectRaceAndBestTime splitFunc lines =
        let values =
            lines
            |> splitFunc

        let (times, distances) = (values |> Seq.head),(values |> Seq.last)
        times
        |> Array.mapi(fun i time ->
            time, distances[i]
        )

    let calculateDistance (pressTime: int64) raceTime =
        if pressTime > raceTime || pressTime <= 0 then
            int64 0
        else
            (raceTime - pressTime) * pressTime

    let beatsRecord (pressTime: int64) raceTime record =
        calculateDistance pressTime raceTime > record

    let rec findFirstWin (pressTime: int64) raceTime record moveTimeFunc =
        match pressTime = 0 || pressTime > raceTime with
        | true -> int64 0 
        | false ->
            let beatRecord = beatsRecord pressTime raceTime record
            match beatRecord with
            | true -> pressTime
            | false -> findFirstWin (moveTimeFunc pressTime) raceTime record moveTimeFunc

    let calculateWins raceTime record = 
        let fastestPressWin = findFirstWin 1 raceTime record ((+) (int64 1))
        let slowestPressWin = findFirstWin (raceTime - int64 1) raceTime record ((+) (int64 -1))
        (slowestPressWin - fastestPressWin)

    let calculateWinsForRaces splitFunc lines =
        lines
        |> collectRaceAndBestTime splitFunc
        |> Array.fold(fun (wins: int64) (raceTime, record) ->
            wins * calculateWins raceTime record
        ) 1 

module Part1 =
    open Boat

    let total =
        calculateWinsForRaces collectValuesAsSeperateNums

module Part2 =
    open Boat

    let total =
        calculateWinsForRaces collectValuesAsSingleNum

input
|> readLines 
|> outputFileResult Part1.total "Part 1"

input
|> readLines 
|> outputFileResult Part2.total "Part 2"
