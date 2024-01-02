open Helper

module Karger =
    open Graph.AdjacencyGraph
    let createGraph lines =
        lines
        |> Seq.map(fun line ->
            line |> String.splitOnStringsTrim [|":";" "|]
        )
        |> Seq.fold(fun g d ->
            let parent, children = d |> Array.head, d |> Array.tail
            children
            |> Array.fold(fun g' c ->
                g' |> addVertex c |> addEdge parent c 1
            ) (g |> addVertex parent)
        ) empty
    
    let compute (graph: AdjacencyGraph<string>) =
        let rec compute'() =
            let rec combinePoints (graph: AdjacencyGraph<string>) = 
                match graph.Vertices.Count with
                | 2 -> graph
                | _ -> 
                    let r = new System.Random()
                    let v1 = graph.VertSet |> Seq.sortBy(fun v -> r.Next()) |> Array.ofSeq |> Array.head
                    let v2 = graph.AllConnections v1 |> Seq.sortBy(fun v -> r.Next()) |> Array.ofSeq |> Array.head
                    graph |> combine v1 v2 |> combinePoints
            let twoVertGraph = combinePoints graph
            let verts = twoVertGraph.VertSet |> Array.ofSeq
            let v1, v2 = verts[0], verts[1]
            let mutable nrCuts = 0
            for part in (v1 |> String.splitOnStringTrim "_" |> Array.distinct) do 
                let outgoingVerts = graph.AllConnections part
                for part2 in (v2 |> String.splitOnStringTrim "_" |> Array.distinct) do
                    if (outgoingVerts.Contains(part2)) then
                        nrCuts <- nrCuts + 1
            match nrCuts with
            | 3 -> v1,v2
            | _ -> 
                compute'()
        compute'()

module Part1 =
    open Karger
    let total lines =
        let (v1,v2) =
            lines
            |> createGraph
            |> compute
        [|v1;v2|],
        (v1 |> String.splitOnString "_" |> Array.distinct |> Array.length) * (v2 |> String.splitOnString "_" |> Array.distinct |> Array.length)

input
|> outputFileSeqAndResult Part1.total "Part 1"
