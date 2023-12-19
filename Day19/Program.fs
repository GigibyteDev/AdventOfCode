open Helper
module GiftWorkflows = 
    type Part = {
        vals: Map<string,int64>
    }
    with
        member this.total = this.vals |> Map.values |> Seq.sum

    type PartRange = {
        valRanges: Map<string,int64*int64>
    }
    with
        static member init = {valRanges = ([|"x";"m";"a";"s"|] |> Array.map(fun k -> k, (1L, 4000L)) |> Map.ofArray) }
        static member zero = {valRanges = ([|"x";"m";"a";"s"|] |> Array.map(fun k -> k, (0L,0L)) |> Map.ofArray) }
        member this.split id compareSymbol cutOff =
            match compareSymbol with
            | '>' -> {valRanges = this.valRanges.Add(id, (cutOff + 1L, snd this.valRanges[id]))},{valRanges = this.valRanges.Add(id, (fst this.valRanges[id], cutOff))}
            | _ -> {valRanges = this.valRanges.Add(id, (fst this.valRanges[id], cutOff - 1L))},{valRanges = this.valRanges.Add(id, (cutOff, snd this.valRanges[id]))}
        member this.total = this.valRanges |> Map.map(fun _ v -> (snd v) - (fst v) + 1L) |> Map.values |> Seq.fold(fun t v -> t * v) 1L

    let retrieveFuncDefinitions (lines: string seq) = 
        let funcStrs = lines |> Seq.filter(fun l -> l |> System.String.IsNullOrWhiteSpace |> not && l.Chars 0 <> '{')
        let fd = 
            funcStrs 
            |> Seq.map(fun l -> 
                l.Trim('}') 
                |> String.splitOnStrings [|"{";","|]
            )
            |> Seq.map(fun f ->
                f[0],
                f[1..]
            ) |> Map.ofSeq
        fd

    let calculateAcceptedParts (funcMap: Map<string, string array>) =
        let rec iterate ranges funcId =
            let (total, _) =
                funcMap[funcId]
                |> Array.fold(fun (total, (remRanges: PartRange)) op ->
                    match op.Contains(':') with
                    | true ->
                        let opAndRes = op |> String.splitOnString ":"
                        let id,compare,v,res = opAndRes[0].Substring(0, 1), opAndRes[0].Chars 1, System.Int64.Parse(opAndRes[0].Substring(2)), opAndRes[1]
                        let newRange, remainder = remRanges.split id compare v
                        match res with
                        | "A" -> total + newRange.total, remainder
                        | "R" -> total, remainder
                        | _ -> total + iterate newRange res, remainder
                    | false ->
                        match op with
                        | "A" -> total + remRanges.total, PartRange.zero
                        | "R" -> total, PartRange.zero
                        | _ -> total + iterate remRanges op, PartRange.zero
                ) (0L,ranges)
            total

        iterate PartRange.init "in"

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

module Part2 = 
    open GiftWorkflows
    let total lines =
        lines
        |> retrieveFuncDefinitions
        |> calculateAcceptedParts

input
|> outputFileResult Part1.total "Part 1"

input
|> outputFileResult Part2.total "Part 2"
