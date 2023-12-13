open Helper

module MagmaMap =
    let rotateMap map =
        seq {
            for i in [0..(map |> Seq.head |> String.length) - 1] do
                yield 
                    seq {
                        for l in (map |> Seq.rev) do
                            yield string l[i] 
                    } |> String.concat ""
        } |> Array.ofSeq
    let collectMap lines =
        let map =
            lines
            |> Array.ofSeq
            |> Array.indexed
            |> Array.fold( fun (groupings, currentCollection) (i,line) ->
                match currentCollection |> Seq.length > 0, line |> System.String.IsNullOrWhiteSpace, i >= (lines |> Seq.length) - 1 with
                | true, true, false ->
                    (groupings @ [currentCollection]), Array.empty
                | true, false, true ->
                    (groupings @ [(Array.append currentCollection [|line|])]), Array.empty
                | _, false, false ->
                    (groupings, Array.append currentCollection [|line|])
                | _ -> (groupings, currentCollection)
            ) (List.empty, Array.empty)
            |> fst
        map

    let compareStringsWithSingleMistake (str1: string) (str2: string) =
        let mutable fixedSmudge = false;
        let doesNotMatch =
            str1 
                |> Seq.indexed 
                |> Seq.exists 
                    (fun (i,_) ->
                        match str1.Chars i = str2.Chars i with
                        | true ->
                             false
                        | false ->
                            match fixedSmudge with
                            | false ->
                                fixedSmudge <- true
                                false
                            | true -> true
                    )
        doesNotMatch |> not, fixedSmudge

    let rec checkReflection (map': (string array)) (index: int) (ripple: int) (smudgeFixed: bool) =
        let index1,index2 = index - ripple, index + ripple - 1
        match index = 0 || index = (map' |> Array.length), index1 < 0, index2 >= (map' |> Array.length) with
        | false, false, false ->
            match smudgeFixed with 
            | true ->
                match map'[index1] = map'[index2] with
                | true ->
                    checkReflection map' index (ripple + 1) smudgeFixed
                | false -> false
            | false ->
                match compareStringsWithSingleMistake map'[index1] map'[index2] with
                | true, f ->
                    checkReflection map' index (ripple + 1) f
                | _ -> false
        | true, _, _ -> false
        | _ ->
            smudgeFixed

    let findReflectionIndex (allowSmudge) (map: (string array)) =
        let reflectionIndex = 
            match map |> Array.indexed |> Array.tryFind(fun (i, m) -> checkReflection map i 1 (allowSmudge |> not)) with
            | Some (i, _) ->
                i * 100
            | None ->
                let rotatedMap = map |> rotateMap
                match rotatedMap |> Array.indexed |> Array.tryFind(fun (i, m) -> checkReflection rotatedMap i 1 (allowSmudge |> not)) with
                | Some (i, _) ->
                    i
                | None ->
                    0
        reflectionIndex

module Part1 =
    open MagmaMap
    let total lines =
        collectMap lines
        |> List.sumBy(fun m -> m |> findReflectionIndex false)
        
module Part2 =
    open MagmaMap
    let total lines =
        collectMap lines
        |> List.sumBy(fun m -> m |> findReflectionIndex true)

input
|> outputFileResult Part1.total "Part 1"

input
|> outputFileResult Part2.total "Part 2"