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

    let outputFileResultSeq (fn: System.String seq -> 't) (title: string) (lines: System.String seq) =
        let timer = Timer.start()
        let res = (fn lines) |> Seq.mapi(fun i r -> i,r)
        let timerStr = timer |> Timer.stopWithOutputString
        printfn $"{title}"
        for (i, r) in res do
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

module Seq =
    let addItem item items =
        Seq.append items (item |> Seq.singleton)



