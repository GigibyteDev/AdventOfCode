open Helper

module OasisTraversal =
    let fileConvert lines =
        lines 
        |> Seq.map(fun line -> 
            line 
            |> String.splitOnStringTrim " " 
            |> Array.map(fun num -> num |> System.Int32.Parse)
            |> List.ofArray
        )

    let seqToDiffs valsSubset =
        valsSubset
        |> Seq.tail
        |> Seq.fold(fun (runningDiffs, prevVal) (curr: int) ->
            (runningDiffs @ [(curr - prevVal)], curr)
        ) (List.empty, (valsSubset |> Seq.head))
        |> fst

    let rec traverseDown (prevLastVal: int option) findValToPass addOrSub valsSubset  =
        match valsSubset |> Seq.groupBy id |> Seq.length = 1 with
        | true -> 
            match prevLastVal with
            | Some lastVal ->
                addOrSub lastVal (valsSubset |> Seq.head) |> Some
            | None -> (valsSubset |> Seq.head) |> Some
        | false ->
            match valsSubset |> Seq.length <= 2 with
            | true -> None
            | false ->
                match (valsSubset |> seqToDiffs) |> traverseDown (valsSubset |> findValToPass |> Some) findValToPass addOrSub with
                | Some newVal ->
                    match newVal with
                    | 0 ->
                        prevLastVal
                    | _ -> 
                        match prevLastVal with
                        | Some lastVal ->
                            addOrSub lastVal newVal |> Some
                        | None ->
                            newVal |> Some
                | None -> None
                
    let findValidSubset findValToPass addOrSub (vals: int list) =
        match vals |> List.head with
        | 0 ->
            ()
        | _ -> ()
        match vals |> traverseDown None findValToPass addOrSub with
        | Some newVal ->
            newVal
        | None ->
            0

module Part1 =
    open OasisTraversal
    let total lines =
        let seq =
            fileConvert lines
            |> Seq.map (findValidSubset (fun vals -> vals |> List.last) (+))
        seq, seq |> Seq.map(fun s -> int64 s) |> Seq.sum

module Part2 =
    open OasisTraversal
    let total lines =
        let seq =
            fileConvert lines
            |> Seq.map (findValidSubset (fun vals -> vals |> List.head) (-))
        seq, seq |> Seq.sum

input
|> outputFileSeqAndResult Part1.total "Part 1"

input
|> outputFileSeqAndResult Part2.total "Part 2"
