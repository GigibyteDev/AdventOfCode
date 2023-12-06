// For more information see https://aka.ms/fsharp-console-apps
let wordsToNumMap =
    [|
        ("one",1)
        ("two",2)
        ("three",3)
        ("four",4)
        ("five",5)
        ("six",6)
        ("seven",7)
        ("eight",8)
        ("nine",9)
    |] |> Map.ofArray

let regexStringForWordAndNumber = $"""(?=({wordsToNumMap |> Map.keys |> Seq.map(fun k -> $"{k}") |> String.concat "|"}|(\d)))"""

let convertMatchToNum (matchVal: string) =
    match matchVal |> System.Int32.TryParse with
    | true, int -> matchVal
    | _ -> 
        match wordsToNumMap |> Map.tryFind matchVal with
        | Some num -> num |> string
        | _ -> ""

let getValOutOfGroups (regexMatch: System.Text.RegularExpressions.Match) =
    (regexMatch.Groups
    |> Seq.filter(fun g -> g.Value |> System.String.IsNullOrWhiteSpace |> not)
    |> Seq.head).Value |> convertMatchToNum

let total =
    Helper.File.readLines "Data\input.txt"
    |> Seq.fold(fun iter l -> 
        let regexMatches = System.Text.RegularExpressions.Regex.Matches(l, $"{regexStringForWordAndNumber}")
        match regexMatches |> Seq.length > 0 with
        | true ->
            match ((regexMatches |> Seq.head |> getValOutOfGroups) + (regexMatches |> Seq.last |> getValOutOfGroups)) |> System.Int32.TryParse with
            | true, int -> iter + int
            | _ -> iter
        | false ->
            iter
    ) 0

printfn $"{total}"
