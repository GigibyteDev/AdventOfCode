open Helper
module SeedMap =
    type SeedRange = {
        StartNum: int64
        EndNum: int64
    }

    type ConversionValue = {
        DestinationRangeStart: int64
        SourceRangeStart: int64
        RangeLength: int64
    }
    with 
        member this.mapNum num =
            match num >= this.SourceRangeStart && num < this.SourceRangeStart + this.RangeLength with
            | true ->
                (true,this.DestinationRangeStart + (num - this.SourceRangeStart))
            | false ->
                (false,num)

        member this.mapNumRange (numRange: SeedRange) =
            seq{
                if (numRange.EndNum < this.SourceRangeStart) then
                    yield (true,numRange)
                elif (numRange.StartNum >= this.SourceRangeStart + this.RangeLength) then
                    yield (true,numRange)
                else
                    if (numRange.StartNum >= this.SourceRangeStart && numRange.EndNum < this.SourceRangeStart + this.RangeLength) then
                        yield (false,{StartNum = this.mapNum numRange.StartNum |> snd; EndNum = this.mapNum numRange.EndNum |> snd})
                    elif (numRange.EndNum < this.SourceRangeStart + this.RangeLength) then
                        yield (true, {StartNum = numRange.StartNum; EndNum = this.SourceRangeStart - int64 1})
                        yield (false, {StartNum = this.SourceRangeStart |> this.mapNum |> snd; EndNum = numRange.EndNum |> this.mapNum |> snd})
                    elif(numRange.StartNum >= this.SourceRangeStart) then
                        yield (true, {StartNum = this.SourceRangeStart + this.RangeLength; EndNum = numRange.EndNum})
                        yield (false, {StartNum = numRange.StartNum |> this.mapNum |> snd; EndNum = this.SourceRangeStart + this.RangeLength - int64 1 |> this.mapNum |> snd})
                    else
                        yield (true, {StartNum = numRange.StartNum; EndNum = this.SourceRangeStart - int64 1})
                        yield (true, {StartNum = this.SourceRangeStart + this.RangeLength; EndNum = numRange.EndNum})
                        yield (false, {StartNum = this.SourceRangeStart |> this.mapNum |> snd; EndNum = this.SourceRangeStart + this.RangeLength - int64 1 |> this.mapNum |> snd})
            } |> Array.ofSeq

        static member init nums =
            match nums |> Array.length = 3 with
            | true ->
                {
                    DestinationRangeStart = nums[0]
                    SourceRangeStart = nums[1]
                    RangeLength = nums[2]
                } |> Some
            | false -> None

    let rec retrieveNewRangesFromConversions (currentRange: SeedRange) (allMaps: ConversionValue array) (iter: int) =
        match iter >= (allMaps |> Array.length) with
        | true ->
            currentRange |> Seq.singleton
        | false ->
            let newRanges = allMaps[iter].mapNumRange currentRange
            match newRanges |> Seq.tryFind(fun (_, nr) -> nr.EndNum < nr.StartNum) with
            | Some r -> ()
            | None -> ()
            let newConvertedSet = (newRanges |> Seq.choose(fun (isOriginal, range) -> match isOriginal with | false -> Some range | true -> None))
            seq {
                yield! newConvertedSet
                for (_, nonConv) in newRanges |> Seq.filter(fun (isOriginal, r) -> isOriginal) do
                    yield! retrieveNewRangesFromConversions nonConv allMaps (iter + 1)
            }

    type ConversionMap = {
        AllMaps: ConversionValue seq
    }
    with
        member this.mapNum num =
            this.AllMaps
            |> Seq.fold(fun (changed,num) map ->
                match changed with
                | true -> (true, num)
                | false ->
                    map.mapNum num
            ) (false, num)
            |> snd

        member this.mapNumRange (numRange: SeedRange) =
            let ranges = retrieveNewRangesFromConversions numRange (this.AllMaps |> Array.ofSeq) 0
            ranges

    let retrieveConversionValue line =
        line
        |> String.splitOnStringTrim " "
        |> Array.map(fun str -> str |> System.Int64.Parse)
        |> ConversionValue.init

    let rec collectMaps (collectedMaps: ConversionMap seq) (runningConversionValues: ConversionValue option seq) (lines: string array) (iter: int) =
        match (iter >= (lines |> Array.length)) with
        | true ->
            collectedMaps |> Seq.addItem {AllMaps = (runningConversionValues |> Seq.choose id)}
        | false ->
            match lines[iter] |> System.String.IsNullOrWhiteSpace || lines[iter].Contains(":") with
            | true ->
                match runningConversionValues |> Seq.length > 0 with
                | true ->
                    collectMaps (collectedMaps |> Seq.addItem {AllMaps = (runningConversionValues |> Seq.choose id)}) Array.empty lines (iter + 1)
                | false ->
                    collectMaps collectedMaps runningConversionValues lines (iter + 1)
            | false ->
                collectMaps collectedMaps (runningConversionValues |> Seq.addItem (retrieveConversionValue lines[iter])) lines (iter + 1)

    let retrieveSeeds line =
        line
        |> String.splitOnStringTrim " "
        |> Array.tail
        |> Array.map(fun str -> str |> System.Int64.Parse)

    let retrieveSeedRanges line =
        let seedStr =
            line
            |> String.splitOnStringTrim " "
            |> Array.tail
        seq {
            for i in [0 .. ((seedStr |> Seq.length) - 2)] do
                if (i % 2 = 0) then
                    let startNum = seedStr[i] |> System.Int64.Parse
                    let range = seedStr[i + 1] |> System.Int64.Parse
                    yield {
                        StartNum = startNum
                        EndNum = (startNum + range - int64 1)
                    }
        }
        |> Array.ofSeq

module Part1 =
    open SeedMap

    let retrieveSeedsAndMaps lines =
        let seeds = lines |> Seq.head |> retrieveSeeds
        let maps = collectMaps Array.empty Array.empty (lines |> Seq.tail |> Array.ofSeq) 0
        seeds,maps

    let total lines =
        let (seeds, maps) =
            lines
            |> retrieveSeedsAndMaps
        seeds
        |> Array.map(fun seed ->
            maps
            |> Seq.fold(fun seed map ->
                map.mapNum seed
            ) seed
        )
        |> Seq.min

module Part2 =
    open SeedMap
    let retrieveSeedRangesAndMaps lines =
        let seeds = lines |> Seq.head |> retrieveSeedRanges
        let maps = collectMaps Array.empty Array.empty (lines |> Seq.tail |> Array.ofSeq) 0
        (seeds,maps)

    let total lines =
        let (seedRanges, maps) = retrieveSeedRangesAndMaps lines
        let min =
            maps
            |> Seq.fold(fun ranges map ->
                ranges
                |> Seq.fold(fun ranges range ->
                    Seq.append ranges (map.mapNumRange range)
                ) Seq.empty
            ) seedRanges
            |> Seq.minBy(fun sr -> sr.StartNum)
        min.StartNum
File.readLines @"Data\input.txt"
|> outputFileResult Part1.total "Part 1"

File.readLines @"Data\input.txt"
|> outputFileResult Part2.total "Part 2"
