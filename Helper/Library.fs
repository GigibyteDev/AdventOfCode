namespace Helper
module Timer =
    open System.Diagnostics

    let start () =     
        let stopWatch = new Stopwatch()
        stopWatch.Start()
        stopWatch

    let stop (stopWatch: Stopwatch) = 
        stopWatch.Stop()
        let ts = stopWatch.Elapsed
        stopWatch.Reset()
        ts

    let stopWithOutputString stopWatch =
        let ts = stop stopWatch
        $"Time to execute: \n\tMinute(s): {ts.Minutes} \n\tSecond(s): {ts.Seconds} \n\tMilliseconds {ts.Milliseconds} \n\tMicroseconds {ts.Microseconds} \n\tNanoseconds {ts.Nanoseconds}"

[<AutoOpen>]
module Output =
    open Spectre.Console

    let lineBreak() = printfn $"============================================================"

    let printf str =
        printf $"{str}"

    let printfn str =
        printfn $"{str}"
    
    let array2dToGrid (grid: 'b array2d) =
        [|
            for y in 0..grid.GetUpperBound 1 do
                yield [|
                    for x in 0..grid.GetUpperBound 0 do
                        yield grid[x,y]
                |]
        |]

    let outputResWithPanel res =
        let panel = Panel(string res)
        panel.Header <- PanelHeader($"Result")
        panel.Border <- BoxBorder.Double
        panel.Padding <- Padding(8, 0, 8, 0)
        panel.BorderColor( Color.Green4 ) |> ignore
        panel
    
    let resOutput (res:obj) =
        match res with
        | null -> "[NULL]"
        | :? string -> $"{res}"
        | :? System.Collections.IEnumerable as seq' -> seq' |> Seq.cast |> Seq.map(fun s -> s |> string) |> String.concat ""
        | _ -> $"{res}"

    let outputResSeqWithPanel (res: 'a seq) =
        let table = new Table()
        table.Border <- TableBorder.Double
        table.Expand <- true
        TableColumn("Iter").RightAligned() |> table.AddColumn |> ignore
        TableColumn("Result").LeftAligned() |> table.AddColumn |> ignore

        for (i,res') in (res |> Seq.mapi(fun i v -> i,v)) do
            [|$"{i + 1}"; $"{res' |> resOutput}"|] |> table.AddRow |> ignore

        table.Title <- TableTitle($"Iterations")
        table

    let createTimerRow color rowTitle value =
        let (prependStr, appendStr) =
            match value with
            | (v) when v > 0 -> $"[{color}]", "[/]"
            | _ -> "[grey]","[/]"
        [|$"{prependStr}{rowTitle}{appendStr}";$"{prependStr}{value}{appendStr}"|]

    let outputTimerWithTable (ts: System.TimeSpan) =
        let table = new Table()
        TableColumn("Interval").RightAligned() |> table.AddColumn |> ignore
        TableColumn("Time").LeftAligned() |> table.AddColumn |> ignore
        ts.Minutes |> createTimerRow "green4" "Minutes" |> table.AddRow |> ignore
        ts.Seconds |> createTimerRow "springgreen4" "Seconds" |> table.AddRow |> ignore
        ts.Milliseconds |> createTimerRow "turquoise4" "Milliseconds" |> table.AddRow |> ignore
        ts.Microseconds |> createTimerRow "deepskyblue3" "Microseconds" |> table.AddRow |> ignore
        ts.Nanoseconds |> createTimerRow "deepskyblue3_1" "Nanoseconds" |> table.AddRow |> ignore
        table.Title <- TableTitle("Time To Run", Style(foreground = Color.Aqua))
        table.BorderColor(Color.Aqua) |> ignore
        table

    let outputResult (title: string) fn =
        let timer = Timer.start()
        let res = fn()
        let ts = timer |> Timer.stop

        let root = new Tree(title)
        let res = root.AddNode(res |> outputResWithPanel)
        res.AddNode(ts |> outputTimerWithTable) |> ignore
        root |> AnsiConsole.Write

    let outputFileResult fn (title: string) lines =
        let timer = Timer.start()
        let res = fn lines
        let ts = timer |> Timer.stop
        let root = new Tree(title)
        let res = 
            match box res with
            | :? System.Collections.IEnumerable as seq' -> root.AddNode (seq' |> Seq.cast |> outputResSeqWithPanel)
            | _ -> root.AddNode (res |> outputResWithPanel)
        res.AddNode (ts |> outputTimerWithTable) |> ignore
        root |> AnsiConsole.Write
        lineBreak()

    let outputFileSeq fn (title: string) lines =
        let timer = Timer.start()
        let res = fn lines 
        let ts = timer |> Timer.stop

        let root = new Tree(title)
        let res = root.AddNode (res |> outputResSeqWithPanel)
        res.AddNode (ts |> outputTimerWithTable) |> ignore
        root |> AnsiConsole.Write
        lineBreak()

    let outputFileSeqAndResult fn (title: string) lines =
        let timer = Timer.start()
        let (seq, res) = fn lines 
        let ts = timer |> Timer.stop
        let root = new Tree(title)
        let res = root.AddNode (res |> outputResWithPanel)
        res.AddNode (seq |> outputResSeqWithPanel) |> ignore
        res.AddNode (ts |> outputTimerWithTable) |> ignore
        root |> AnsiConsole.Write
        lineBreak()

[<AutoOpen>]
module File =
    let readLines filePath = 
        try
            System.IO.File.ReadLines(System.IO.Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().Location) + @"\" + filePath)
        with
            | :? System.IO.FileNotFoundException as msg-> 
                printfn msg.Message
                Set.empty
    let input = @"Data\input.txt" |>  readLines
    let test = @"Data\testInput.txt" |> readLines

    let createCharGrid lines = lines |> Seq.map(fun (line: string) -> line.ToCharArray()) |> Array.ofSeq
    let createIntGrid lines = lines |> Seq.map(fun (line: string) -> line |> Seq.map(fun c -> c |> string |> System.Int32.Parse) |> Array.ofSeq) |> Array.ofSeq

module String =    
    open System

    let splitOnString (splitStr:string) (string:String) =
        string.Split([|splitStr|], StringSplitOptions.None)

    let splitOnStringTrim (splitStr:string) (string:String) =
        string.Split([|splitStr|], (StringSplitOptions.TrimEntries + StringSplitOptions.RemoveEmptyEntries))

    let splitOnStrings (splitStr:string seq) (string:String) =
        string.Split(splitStr |> Array.ofSeq, StringSplitOptions.None)

    let splitOnStringsTrim (splitStr:string seq) (string:String) =
        string.Split(splitStr |> Array.ofSeq, (StringSplitOptions.TrimEntries + StringSplitOptions.RemoveEmptyEntries))

    let replace oldVal (newVal: String) (string: String) =
        string.Replace(oldVal, newVal)

    let isDigit = System.Char.IsDigit

    let isStringDigit (str: string) = str.Chars 0 |> isDigit

    let trimChars (trimStr: string seq) (string: String) =
        trimStr
        |> Seq.fold (fun (str:string) (trim:string) ->
            str.Trim(trim.Chars 0)
        ) (string.Trim())

module Seq =
    type ListMap<'a,'b when 'a : equality> = {
        vals: ('a*'b) List
    }
    with
        member this.Add (key: 'a) (value: 'b) =
            match this.vals |> List.tryFind(fun (k, _) -> k = key) with
            | Some _ ->
                {vals = this.vals |> List.map(fun (k, v) -> match k = key with | true -> k,value | false -> k,v)}
            | None ->
                {vals = this.vals @ [(key,value)]}

        member this.Remove (key: 'a) =
            {vals = this.vals |> List.filter(fun (k,_) -> k = key |> not)}
        member this.TryFind (key: 'a) = this.vals |> List.tryFind(fun (k, _) -> k = key)
        static member tryFind (key: 'a) (this: ListMap<'a,'b>) = this.vals |> List.tryFind(fun (k, _) -> k = key)
        static member add (key: 'a) (value: 'b) (this: ListMap<'a,'b>)=
            match this.vals |> List.tryFind(fun (k, v) -> k = key) with
            | Some _ ->
                {vals = this.vals |> List.map(fun (k, v) -> match k = key with | true -> k,value | false -> k,v)}
            | None ->
                {vals = this.vals @ [(key,value)]}

        static member remove (key: 'a) (this: ListMap<'a,'b>) =
            {vals = this.vals |> List.filter(fun (k,_) -> k = key |> not)}

        static member empty =
            let t: List<('a*'b)> = List.empty
            {vals = t}

    let addItem item items =
        Seq.append items (item |> Seq.singleton)

module GridNav =
    type Direction =
    | North
    | South
    | East
    | West
    with
        member this.rev =
            match this with
            | North -> South
            | South -> North
            | West -> East
            | East -> West

    let private nextCoord' (gridWidth: int) (gridHeight: int) dir loops rev (coords: (int*int))=
        let (x,y) = coords
        let (x',y') =
            match dir with
            | North -> (x, y - 1)
            | South -> (x, y + 1)
            | East -> (x + 1, y)
            | West -> (x - 1, y)
        match dir with
        | North
        | South ->
            match y' with
            | y when y < 0 || y >= gridHeight ->
                match loops with
                | true ->
                    let loopAdd = (match rev with | true -> -1 | false -> 1)
                    match x' with 
                    | x when x + loopAdd < 0 || x + loopAdd >= gridWidth -> None
                    | x -> (x + loopAdd, match dir with | North -> gridHeight - 1 | _ -> 0) |> Some
                | false -> None
            | y -> (x', y') |> Some
        | East
        | West ->
            match x' with
            | x when x < 0 || x >= gridWidth ->
                match loops with
                | true ->
                    let loopAdd = (match rev with | true -> -1 | false -> 1)
                    match y' with
                    | y when x + loopAdd < 0 || y + loopAdd >= gridHeight -> None
                    | y -> ((match dir with | West -> gridWidth - 1 | _ -> 0), y + loopAdd) |> Some
                | false -> None
            | x -> (x', y') |> Some

    let nextCoordLoop (grid: 'T array array) dir coords = nextCoord' grid[0].Length grid.Length dir true false coords
    let nextCoordRev (grid: 'T array array) dir coords = nextCoord' grid[0].Length grid.Length dir true true coords
    let nextCoord (grid: 'T array array) dir coords = nextCoord' grid[0].Length grid.Length dir false false coords

    let nextCoordLoopXY gridWidth gridHeight dir coords = nextCoord' gridWidth gridHeight dir true false coords
    let nextCoordXY gridWidth gridHeight dir coords = nextCoord' gridWidth gridHeight dir false false coords

    let rec loopThroughGrid' (coords: (int*int) option) (grid: 'T array array) (func: (int*int) -> ('T array array) -> 'a -> 'a) (res: 'a) =
        match coords with
        | Some c ->
            loopThroughGrid' (c |> nextCoordLoop grid East) grid func (func c grid res)
        | None ->
            res

    let loopThroughGrid (grid: 'T array array) func init = loopThroughGrid' ((0,0) |> Some) grid func init

    let loopThroughGrid2 (grid: 'T array array) func val1 init = 
        let (_, res) = loopThroughGrid' ((0,0) |> Some) grid func (val1, init)
        res

    let loopThroughGrid3 (grid: 'T array array) func val1 val2 init = 
        let (_, _, res) = loopThroughGrid' ((0,0) |> Some) grid func (val1, val2, init)
        res

    let at coord (grid: 'b array array) =
        let (x, y) = coord
        grid[y][x]

module Math =
    let rec gcd a b = 
        match (a,b) with
        | (x,0L) -> x
        | (x,y) when x = y -> x
        | (a,b) -> gcd b (a % b)

    let lcm a b = 
        let gcd = gcd a b
        (int64 a * int64 b) / int64 gcd

    let lcmMultiple (vals: int seq) =
        vals
        |> Seq.tail
        |> Seq.fold(fun runningLCM val' ->
            lcm runningLCM val'
        ) (vals |> Seq.head |> int64)