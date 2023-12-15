open Helper
module Hashing =
    open Helper.Seq

    type Operation =
    | Remove of boxAndlabel: (int*string)
    | Add of boxlabelAndAmt: (int*string*int)

    let retrieveItems lines =
        lines
        |> Seq.head
        |> String.splitOnString ","

    let calculateHash (str: string) =
        (System.Text.Encoding.ASCII.GetBytes(str) |> Array.map(fun b -> int b))
        |> Array.fold(fun currentValue asciiCode ->
            (((currentValue + asciiCode) * 17) % 256)
        ) 0

    let convertItemToOperation (line: string) =
        match line with
        | item when item.Contains('=') ->
            let labelAndAmt = item |> String.splitOnString "="
            Add(labelAndAmt |> Array.head |> calculateHash, labelAndAmt |> Array.head, labelAndAmt |> Array.last |> System.Int32.Parse) |> Some
        | item when item.Contains('-') ->
            let label = item |> String.splitOnString "-" |> Array.head
            Remove(label |> calculateHash, label) |> Some
        | _ -> None
    

    let sumMapValues (map: Map<int,ListMap<string,int>>) =
        map
        |> Map.fold(fun total' boxNum mirrors ->
            mirrors.vals
            |> List.indexed
            |> List.fold(fun total (mirrorNum, (_, focalLength)) ->
                total + ((boxNum + 1) * (mirrorNum + 1) * focalLength)
            ) total'
        ) 0

    let findValuesFromOperations (instructions': Operation seq) =
        let rec readInstructions (map: Map<int, ListMap<string,int>>) (ins: Operation list) =
            match ins with
            | [] -> map
            | cs :: tail ->
                match cs with
                | Remove (box, label) -> 
                    match map |> Map.tryFind box with
                    | Some labels -> 
                        readInstructions (map.Add(box, labels.Remove(label))) tail
                    | None -> readInstructions map tail
                | Add (box, label, amt) -> 
                    match map |> Map.tryFind box with
                    | Some labels -> 
                        readInstructions (map.Add(box, labels.Add label amt)) tail
                    | None -> readInstructions (map.Add(box, ListMap.empty.Add label amt)) tail

        readInstructions Map.empty (instructions' |> List.ofSeq)
        |> sumMapValues

module Part1 =
    open Hashing
    let total lines =
        retrieveItems lines
        |> Seq.sumBy(fun l -> calculateHash l)

module Part2 =
    open Hashing
    let total lines =
        retrieveItems lines
        |> Array.choose convertItemToOperation
        |> findValuesFromOperations

input
|> outputFileResult Part1.total "Part 1"

input
|> outputFileResult Part2.total "Part 2"