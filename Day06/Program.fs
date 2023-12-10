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

    let calculateDistance pressTime raceTime =
        if pressTime > raceTime || pressTime <= 0L then
            0L
        else
            (raceTime - pressTime) * pressTime

    let beatsRecord pressTime raceTime record =
        calculateDistance pressTime raceTime > record

    let rec findFirstWin pressTime raceTime record moveTimeFunc =
        match pressTime = 0L || pressTime > raceTime with
        | true -> 0L 
        | false ->
            let beatRecord = beatsRecord pressTime raceTime record
            match beatRecord with
            | true -> pressTime
            | false -> findFirstWin (moveTimeFunc pressTime) raceTime record moveTimeFunc

    let calculateWins raceTime record = 
        let shortestPressWin = findFirstWin 1 raceTime record ((+)1L)
        let longestPressWin = findFirstWin (raceTime - 1L) raceTime record ((+)(-1L))
        (longestPressWin - shortestPressWin + 1L)

    let calculateWinsForRaces splitFunc lines =
        lines
        |> collectRaceAndBestTime splitFunc
        |> Array.fold(fun wins (raceTime, record) ->
            wins * calculateWins raceTime record
        ) 1L 

module Part1 =
    open Boat

    let total =
        calculateWinsForRaces collectValuesAsSeperateNums

module Part2 =
    open Boat

    let total =
        calculateWinsForRaces collectValuesAsSingleNum

input
|> outputFileResult Part1.total "Part 1"

input
|> outputFileResult Part2.total "Part 2"
