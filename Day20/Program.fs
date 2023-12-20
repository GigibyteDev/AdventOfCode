open Helper

module Broadcaster =
    open System.Collections.Generic
    
    type Pulse = {
        isHigh: bool
        originId: string
    }

    type ModuleType =
    | FlipFlop of on:bool
    | Conjunction of mem:Map<string,bool>
    | Broadcaster

    type Module = {
        id: string
        moduleType: ModuleType
        connectedIds: string seq
    }

    let mutable loopCounts = Map.empty // Quick and dirty way of tracking the loops
    
    let calcLCM x =
        loopCounts |> Map.map(fun k l -> match l with | (h :: []) -> 0 | [] -> 0 | (h :: t) -> (t |> List.head) - h) |> Seq.fold (fun t v -> int64 t * int64 v.Value ) 1

    let retrieveModules lines =
        let presetConjunctions =
            lines
            |> Seq.map(fun (line:string) -> 
                let mdata = line |> String.splitOnStringsTrim [|" ";"-";">";","|]
                let id = mdata[0].Substring(1)
                id, {id = id; moduleType = (match mdata[0][0] with | '%' -> FlipFlop(false) | '&' -> Conjunction(Map.empty) | _ -> Broadcaster); connectedIds = mdata[1..]}
            ) |> Map.ofSeq
        presetConjunctions
        |> Map.map(fun key m ->
            match m.moduleType with
            | Conjunction _ ->
                let connectedMods = presetConjunctions |> Map.values |> Seq.filter(fun m' -> m'.connectedIds |> Seq.contains m.id) |> Seq.map(fun m' -> m'.id, false) |> Map.ofSeq
                {m with moduleType = Conjunction(connectedMods)}
            | _ -> m
        )

    let pushButton state totalPresses =
        let buttonPress = {isHigh = false; originId = "button"}
        let queue = Queue<(Pulse*string)>()
        queue.Enqueue(buttonPress, "roadcaster")

        let retrievePulseAndUpdateState (s: Map<string,Module>) id (pulse: Pulse) =
            match id with
            | "output" -> s, None
            | _ ->
                match s |> Map.tryFind id with 
                | None -> s, None 
                | Some _ -> 
                    match s[id].moduleType with
                    | FlipFlop on -> match pulse.isHigh with | true -> s, None | false -> s.Add(id, {s[id] with moduleType = FlipFlop(on |> not)}), Some {isHigh = on |> not; originId = id}
                    | Conjunction mem -> 
                        let newMem = mem.Add(pulse.originId, pulse.isHigh)
                        s.Add(id, {s[id] with moduleType = Conjunction(newMem)}), Some {isHigh = (newMem |> Seq.forall(fun v -> v.Value)) |> not; originId = id}
                    | Broadcaster -> s, Some {pulse with originId = id}
        
        let rec pulse' (lowCount,highCount) state =
            match queue.TryDequeue() with
            | false, _ -> (lowCount,highCount), state
            | true, (pulse, id) ->
                match pulse.isHigh, loopCounts |> Map.tryFind pulse.originId with 
                | true, Some pc -> 
                    loopCounts <- loopCounts.Add(pulse.originId, totalPresses :: pc)
                | _ -> ()
                let (nlowCount, nhighCount) = match pulse.isHigh with | true -> (lowCount, highCount + 1) | false -> (lowCount + 1, highCount)
                let (newState, newPulse) = retrievePulseAndUpdateState state id pulse
                match newPulse with 
                | Some p -> 
                    newState[id].connectedIds |> Seq.iter(fun childId -> queue.Enqueue(p, childId)) |> ignore
                    pulse' (nlowCount, nhighCount) newState
                | None ->
                    pulse' (nlowCount, nhighCount) newState
        pulse' (0,0) state 
    
    let beginCyclePulses amt (state: Map<string,Module>) =
        loopCounts <- 
            (match (state |> Map.tryPick(fun k v -> match v.connectedIds |> Seq.contains "rx" with | true -> Some v | false -> None) |> Option.defaultValue state["roadcaster"]).moduleType with
            | Conjunction mem -> mem.Keys |> Seq.map(fun k -> k, List.empty)
            | _ -> Seq.empty)
            |> Map.ofSeq
        let rec cyclePulses totalPresses lows highs (state: Map<string,Module>) =
            match totalPresses > amt with
            | true ->
                lows,highs,state
            | _ -> 
                let ((lowCount, highCount), state) = pushButton state totalPresses
                cyclePulses (totalPresses + 1) (lows + lowCount) (highs + highCount) state

        cyclePulses 0 0 0 state

    let retrieveTotal (lows, highs, state) = lows * highs

module Part1 =
    open Broadcaster
    let total lines =
        lines
        |> retrieveModules
        |> beginCyclePulses 1000
        |> retrieveTotal

module Part2 =
    open Broadcaster
    let total lines =
        lines
        |> retrieveModules
        |> beginCyclePulses 10000
        |> calcLCM

input
|> outputFileResult Part1.total "Part 1"

input
|> outputFileResult Part2.total "Part 2"
