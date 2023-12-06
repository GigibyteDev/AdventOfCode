open Helper

module Cube =
    type Cube =
    | Blue
    | Red
    | Green

    let stringToCubeMap = [| ("blue", Blue); ("red", Red); ("green", Green) |] |> Map.ofArray

module Part1 =
    open Cube
    let gameHasBadPulls badCondition (pulls: string seq) =
        pulls
        |> Seq.map(fun pull -> 
            let amtAndColor = pull |> String.splitOnString " "
            amtAndColor |> Seq.head |> System.Int32.Parse, stringToCubeMap[amtAndColor |> Seq.last]
        )
        |> Seq.exists(fun (amt, cube) -> badCondition cube amt)

    let extractGamesFromLine badCondition (line: string) =
        let gameAndPulls = line |> String.splitOnStringsTrim [|":";";";","|]
        match gameHasBadPulls badCondition (gameAndPulls |> Seq.tail) with
        | true ->
            0
        | false ->
            (gameAndPulls |> Seq.head) |> String.splitOnString " " |> Seq.last |> System.Int32.Parse

    let retrieveSumOfBadGames badCondition (gameLines: string seq) =
        gameLines
        |> Seq.fold(fun total line ->
                total + (line |> extractGamesFromLine badCondition)
            ) 0

    let badCondition cube amt =
        match cube with
        | Blue ->
            amt > 14
        | Green ->
            amt > 13
        | Red ->
            amt > 12
    
    let total =
        File.readLines "Data\input.txt"
        |> retrieveSumOfBadGames badCondition

Part1.total |> System.Console.WriteLine

module Part2 =
    open Cube

    let sanitizeLine line =
        line 
        |> String.splitOnStringsTrim [|":";",";";"|]
        |> Seq.tail
        |> Seq.map(fun r -> 
            let amtAndCube = r |> String.splitOnString " "
            (amtAndCube |> Seq.head) |> System.Int32.Parse, stringToCubeMap[(amtAndCube |> Seq.last)]
        )
        |> Seq.groupBy(fun (_, cube) -> cube )
        |> Seq.map(fun (_, v) -> v |> Seq.map(fun (amt, _) -> amt))
        |> Seq.map(fun v -> v |> Seq.max)

    let calculateTotalPower lines =
        lines
        |> Seq.map (fun line ->
            line 
            |> sanitizeLine
            |> Seq.fold (fun power amt -> match power with | 0 -> amt | _ -> power * amt) 0
        )
        |> Seq.sum

    let total =
        File.readLines "Data\input.txt"
        |> calculateTotalPower

