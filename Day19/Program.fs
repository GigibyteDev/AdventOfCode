open Helper
module GiftWorkflows = 
    type Part = {
        vals: Map<string,int64>
    }
    with
        member this.total = this.vals |> Map.values |> Seq.sum

    let buildFuncsAndParts lines =
        let buildPart (line:string) =
            let partVals =
                line.Trim([|'{';'}'|]) |> String.splitOnString ","
                |> Array.map(fun v -> 
                    let nameAndVal = v |> String.splitOnString "="
                    nameAndVal[0],(nameAndVal[1] |> System.Int64.Parse)
                ) |> Map.ofArray
            { vals = partVals }
        let buildFunc (line: string) =
            let funcData = line.Trim('}') |> String.splitOnString "{"
            let (funcName, funcOps) = funcData[0], funcData[1] |> String.splitOnString ","
            let func =
                fun part ->
                    let res = 
                        funcOps
                        |> Array.pick(fun op ->
                            match op.Contains(":") with
                            | true -> 
                                let opd = op.Split(":")
                                let (comparison, res) = opd[0], opd[1]
                                match comparison.Contains("<") with
                                | true ->
                                    let cd = comparison.Split("<")
                                    match part.vals[cd[0]] < (System.Int64.Parse(cd[1])) with
                                    | true -> Some(res)
                                    | false -> None
                                | false ->
                                    let cd = comparison.Split(">")
                                    match part.vals[cd[0]] > (System.Int64.Parse(cd[1])) with
                                    | true -> Some(res)
                                    | false -> None
                            | false -> Some(op)
                        )
                    match res with
                    | "A" -> None, true
                    | "R" -> None, false
                    | _ -> Some(res), false
            funcName, func
        let parts = 
            lines
            |> Seq.filter(fun (l:string) -> (l |> System.String.IsNullOrWhiteSpace |> not) && l.Chars 0 = '{')
            |> Seq.map(fun l -> l |> buildPart)
            |> List.ofSeq
        let funcs = 
            lines
            |> Seq.filter(fun l -> (l |> System.String.IsNullOrWhiteSpace |> not) && l.Chars 0 <> '{')
            |> Seq.map(fun l -> l |> buildFunc)
            |> Map.ofSeq
        parts, funcs

    let processParts (parts, (funcs: Map<string,(Part -> string option*bool)>)) =

        let rec processPart part funcName =
            match funcs[funcName] part with
            | Some n, _ -> processPart part n
            | None, b -> b

        parts 
        |> List.filter(fun p -> processPart p "in")
        |> List.sumBy(fun p -> p.total)

module Part1 = 
    open GiftWorkflows
    let total lines =
        lines 
        |> buildFuncsAndParts 
        |> processParts

input
|> outputFileResult Part1.total "Part 1"
