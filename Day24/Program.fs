open Helper

module HailStorm =
    open MathNet.Numerics.LinearAlgebra
    open Microsoft.Z3

    type Hail = {
        px: decimal
        py: decimal
        pz: decimal
        vx: decimal
        vy: decimal
        vz: decimal
    }

    let createHail lines =
        lines
        |> Seq.map(fun line -> 
            let vals = line |> String.splitOnStringsTrim [|",";"@"|] |> Array.map(fun v -> System.Decimal.Parse v)
            {px = vals[0]; py = vals[1]; pz = vals[2]; vx = vals[3]; vy = vals[4]; vz = vals[5]}
        ) |> Array.ofSeq

    let calculateIntersectionXY (h1: Hail) (h2: Hail) =
        let slope1 = match h1.vx with | 0m -> 0m | _ -> h1.vy / h1.vx
        let slope2 = match h2.vx with | 0m -> 0m | _ -> h2.vy / h2.vx

        match slope1 = slope2 with
        | true ->
            None
        | false ->
            let x = (h2.py - h1.py + (slope1 * h1.px) - (slope2 * h2.px)) / (slope1 - slope2)
            let y = h1.py + slope1 * (x - h1.px)
            Some(x, y)

    let retrieveAllIntersections (hail: Hail[]) =
        hail
        |> Array.mapi(fun i h1 -> i,h1)
        |> Array.collect(fun (i,h1) ->
            let tailHails = hail |> Array.splitAt(i + 1) |> snd
            tailHails
            |> Array.choose(fun h2 -> calculateIntersectionXY h1 h2 |> Option.map(fun v -> (h1,h2,v)))
        )
    
    let crossInFuture ((h1:Hail),(h2:Hail),(ix, iy)) =
        let h1X = match h1.vx > 0m with | true -> ix > h1.px | _ -> ix < h1.px
        let h2X = match h2.vx > 0m with | true -> ix > h2.px | _ -> ix < h2.px
        let h1Y = match h1.vy > 0m with | true -> iy > h1.py | _ -> iy < h1.py
        let h2Y = match h2.vy > 0m with | true -> iy > h2.py | _ -> iy < h2.py
        h1X && h2X && h1Y && h2Y

    let intersectionInBounds boundLow boundHigh (ix, iy) =
        ix >= boundLow && ix <= boundHigh && iy >= boundLow && iy <= boundHigh

    let linearMatrixSolve hail' =
        // This equation picked from mvorber's solve from comment: https://www.reddit.com/r/adventofcode/comments/18pnycy/comment/ket2703/?utm_source=share&utm_medium=web2x&context=3
        // Continuing to research this...

        let hail = hail' |> Array.take 3
        let pick3 = Seq.toArray >> fun a -> (a[0], a[1], a[2])
        let (h1, h2, h3) = hail[0],hail[1],hail[2]
        let (h1x, h1y, h1z) =      (double h1.px,double h1.py,double h1.pz)
        let (h1vx, h1vy, h1vz) =   (double h1.vx,double h1.vy,double h1.vz)
        let (h2x, h2y, h2z) =      (double h2.px,double h2.py,double h2.pz)
        let (h2vx, h2vy, h2vz) =   (double h2.vx,double h2.vy,double h2.vz)
        let (h3x, h3y, h3z) =      (double h3.px,double h3.py,double h3.pz)
        let (h3vx, h3vy, h3vz) =   (double h3.vx,double h3.vy,double h3.vz)
        let M = matrix [|
            [ h1vy - h2vy; h2vx - h1vx; 0;         h2y - h1y; h1x - h2x; 0       ]
            [ h1vy - h3vy; h3vx - h1vx; 0;         h3y - h1y; h1x - h3x; 0       ]
            [ h2vz - h1vz; 0;         h1vx - h2vx; h1z - h2z; 0;       h2x - h1x ]
            [ h3vz - h1vz; 0;         h1vx - h3vx; h1z - h3z; 0;       h3x - h1x ]
            [ 0;         h1vz - h2vz; h2vy - h1vy; 0;       h2z - h1z; h1y - h2y ]
            [ 0;         h1vz - h3vz; h3vy - h1vy; 0;       h3z - h1z; h1y - h3y ]
        |]
        let b = vector [
            (h2y * h2vx - h2x * h2vy) - (h1y * h1vx - h1x * h1vy)
            (h3y * h3vx - h3x * h3vy) - (h1y * h1vx - h1x * h1vy)
            (h2x * h2vz - h2z * h2vx) - (h1x * h1vz - h1z * h1vx)
            (h3x * h3vz - h3z * h3vx) - (h1x * h1vz - h1z * h1vx)
            (h2z * h2vy - h2y * h2vz) - (h1z * h1vy - h1y * h1vz)
            (h3z * h3vy - h3y * h3vz) - (h1z * h1vy - h1y * h1vz)
        ]

        let OU = M.Solve(b) |> Vector.toArray |> Array.map (fun v -> System.Math.Round(v) |> int64)

        OU[0] + OU[1] + OU[2]
    
    let alignmentSearchSolve hail' =
        let solveForZ x nh0 nh1 = 
            let v = (x - nh0.px) / nh0.vx
            let w = (x - nh1.px) / nh1.vx
            (nh0.pz - nh1.pz + v * nh0.vz - w * nh1.vz) / (v - w)

        // Inspired by Day 24 solutions reddit thread
        let hail = hail' |> Array.take 5
        let (h0,h1,h2,h3,h4) = hail[0],hail[1],hail[2],hail[3],hail[4]
        let y =  19m      //    for y in -100m..1000m do // Loop through to find valid offset
        let x = 314m       //    for x in -100m..1000m do
        let nh0, nh1, nh2, nh3, nh4 = 
            { h0 with vx = (h0.vx - x); vy = (h0.vy - y) }, 
            { h1 with vx = (h1.vx - x); vy = (h1.vy - y) }, 
            { h2 with vx = (h2.vx - x); vy = (h2.vy - y) }, 
            { h3 with vx = (h3.vx - x); vy = (h3.vy - y) }, 
            { h4 with vx = (h4.vx - x); vy = (h4.vy - y) } 
        let i0 = calculateIntersectionXY nh0 nh1 |> Option.map(fun (x, y) -> System.Math.Round x, System.Math.Round y)
        let i1 = calculateIntersectionXY nh0 nh2 |> Option.map(fun (x, y) -> System.Math.Round x, System.Math.Round y)
        let i2 = calculateIntersectionXY nh0 nh3 |> Option.map(fun (x, y) -> System.Math.Round x, System.Math.Round y)
        let i3 = calculateIntersectionXY nh0 nh4 |> Option.map(fun (x, y) -> System.Math.Round x, System.Math.Round y)
        match i0, i1, i2, i3 with
        | Some (i0x,i0y),Some (i1x,i1y),Some (i2x,i2y),Some (i3x,i3y) ->
            if ((i0x, i0y) = (i1x, i1y) && (i1x, i1y) = (i2x, i2y) && (i2x, i2y) = (i3x, i3y)) then
                let z = solveForZ i0x nh0 nh1
                i0x + i0y + (nh0.pz + (i0x - nh0.px) * (nh0.vz - z) / nh0.vx)
            else 0m
        | _ -> 
            0m
                
    let z3Solve hail' =
        let ctx = new Microsoft.Z3.Context()
        // TODO
        0m

module Part1 =
    open HailStorm
    let total lines =
        let hailIntersections =
            lines
            |> createHail
            |> retrieveAllIntersections
            |> Array.filter(fun v -> crossInFuture v)
            |> Array.filter(fun (_,_,v) -> intersectionInBounds 200000000000000m 400000000000000m v)
        hailIntersections |> Array.length

module Part2 =
    open HailStorm
    let total lines =
        let hail =
            lines
            |> createHail
            |> alignmentSearchSolve
        hail

input
|> outputFileResult Part1.total "Part 1"

input
|> outputFileResult Part2.total "Part 2"