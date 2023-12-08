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

    let lineBreak = printfn $"============================================================"

    let printf str =
        printf $"{str}"

    let printfn str =
        printfn $"{str}"

    let outputResWithPanel res =
        let panel = Panel(string res)
        panel.Header <- PanelHeader($"Result")
        panel.Border <- BoxBorder.Double
        panel.Padding <- Padding(8, 0, 8, 0)
        panel.BorderColor( Color.Green4 ) |> ignore
        panel

    let outputResSeqWithPanel (res: 'a seq) =
        let table = new Table()
        table.Border <- TableBorder.Double
        TableColumn("Iter").RightAligned() |> table.AddColumn |> ignore
        TableColumn("Result").LeftAligned() |> table.AddColumn |> ignore

        for (i,res') in (res |> Seq.mapi(fun i v -> i,v)) do
            [|$"{i + 1}"; $"{res'}"|] |> table.AddRow |> ignore

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
        root.AddNode(res |> outputResWithPanel) |> ignore
        root.AddNode(ts |> outputTimerWithTable) |> ignore

        root |> AnsiConsole.Write

    let outputFileResult fn (title: string) lines =
        let timer = Timer.start()
        let res = fn lines
        let ts = timer |> Timer.stop
        let root = new Tree(title)
        let res = root.AddNode (res |> outputResWithPanel)
        res.AddNode (ts |> outputTimerWithTable) |> ignore
        root |> AnsiConsole.Write
        lineBreak

    let outputFileResultSeq fn (title: string) lines =
        let timer = Timer.start()
        let res = fn lines 
        let ts = timer |> Timer.stop

        let root = new Tree(title)
        let res = root.AddNode (res |> outputResSeqWithPanel)
        res.AddNode (ts |> outputTimerWithTable) |> ignore
        root |> AnsiConsole.Write
        lineBreak

    let outputFileSeqAndResult fn (title: string) lines =
        let timer = Timer.start()
        let (seq, res) = fn lines 
        let ts = timer |> Timer.stop
        let root = new Tree(title)
        let res = root.AddNode (res |> outputResWithPanel)
        res.AddNode (seq |> outputResSeqWithPanel) |> ignore
        res.AddNode (ts |> outputTimerWithTable) |> ignore
        root |> AnsiConsole.Write
        lineBreak

[<AutoOpen>]
module File =
    let input = @"Data\input.txt"

    let test = @"Data\testInput.txt"

    let readLines filePath = System.IO.File.ReadLines(System.IO.Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().Location) + @"\" + filePath)

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
    let addItem item items =
        Seq.append items (item |> Seq.singleton)

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



