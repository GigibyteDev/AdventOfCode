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

    let rec calculateWins (pressTime: int64) raceTime record runningWins =
        match pressTime = 0 with 
        | true -> runningWins 
        | false -> 
            let beatRecord = beatsRecord pressTime raceTime record
            if (beatRecord || runningWins = 0) then
                calculateWins (pressTime - int64 1) raceTime record (runningWins + (match beatRecord with | true -> 1 | false -> 0))
            else
                runningWins

    let calculateWinsForRaces splitFunc lines =
        lines
        |> collectRaceAndBestTime splitFunc
        |> Array.fold(fun wins (raceTime, record) ->
            wins * calculateWins (raceTime - int64 1) raceTime record 0
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
