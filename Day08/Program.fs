open Helper

module MapNav =
    let indexMap =[|"L",0;"R",1|] |> Map.ofArray

    let retrieveDirections (line: string) =
        line
        |> Seq.map(fun c -> indexMap[(string c)])
        |> Array.ofSeq
    
    let retrieveDirectionsAndMappings (lines: string seq) =
        let dir = lines |> Seq.head |> retrieveDirections
        dir,lines
        |> Seq.tail
        |> Seq.filter(fun line -> line |> System.String.IsNullOrWhiteSpace |> not)
        |> Seq.map(fun line ->
            let keyAndVals = line |> String.splitOnStringTrim "="
            keyAndVals |> Array.head,
            keyAndVals 
            |> Array.last
            |> String.trimChars [|"(";")"|]
            |> String.splitOnStringTrim ","
        )
        |> Map.ofSeq
    
    let rec followMap (totalSteps: int) (currentLoc: string) (directionsSeq: int array) (dirIter: int) matchFunc (mapping: Map<string,string array>) =
        if matchFunc currentLoc then
            totalSteps
        else
            let dirIter' = (match dirIter >= (directionsSeq |> Seq.length) with | true -> 0 | false -> dirIter)
            followMap (totalSteps + 1) (mapping[currentLoc][directionsSeq[dirIter']]) directionsSeq (dirIter' + 1) matchFunc mapping

    let beginFollowingMap dirAndMapping =
        followMap 0 "AAA" (dirAndMapping |> fst) 0 (fun v -> v = "ZZZ") (dirAndMapping |> snd)

    let retrieveAllZLocations (dirAndMapping: (int array*Map<string, string array>)) =
        let startingLocs = (dirAndMapping |> snd).Keys |> Seq.filter(fun k -> k |> Seq.last |> string = "A") |> Array.ofSeq
        startingLocs
        |> Array.map(fun start ->
            followMap 0 start (dirAndMapping |> fst) 0 (fun v -> v |> Seq.last |> string = "Z") (dirAndMapping |> snd)
        )

module Part1 =
    open MapNav
    let total lines =
        retrieveDirectionsAndMappings lines 
        |> beginFollowingMap

module Part2 =
    open MapNav
    let total lines =
        let seq =
            retrieveDirectionsAndMappings lines
            |> retrieveAllZLocations
        seq, seq |> Math.lcmMultiple

input
|> outputFileResult Part1.total "Part 1"

input
|> outputFileSeqAndResult Part2.total "Part 2"
