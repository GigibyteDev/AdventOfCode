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
    let printf str =
        printf $"{str}"

    let printfn str =
        printfn $"{str}"

    let outputResult (title: string) fn =
        let timer = Timer.start()
        let res = fn()
        let timerStr = timer |> Timer.stopWithOutputString
        printfn $"{title}: {res}"
        printfn timerStr

    let outputFileResult fn (title: string) lines =
        let timer = Timer.start()
        let res = fn lines
        let timerStr = timer |> Timer.stopWithOutputString
        printfn $"{title}: {res}"
        printfn timerStr

    let outputFileResultSeq fn (title: string) lines =
        let timer = Timer.start()
        let res = fn lines 
        let timerStr = timer |> Timer.stopWithOutputString
        printfn $"{title}"
        for (i, r) in res|> Seq.mapi(fun i r -> i,r) do
            printfn $"{i}: {r}"
        printfn timerStr

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



