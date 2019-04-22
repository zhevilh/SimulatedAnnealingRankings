open KendallTauDistance
open SimulatedAnnealing

let r1 = [[3]; [6;5]; [11]; [12;4;1;8;7;13;2;10;9]]
let r2 = [[3]; [2;10]; [5]; [11;6;4;13]; [7]; [8]; [12]; [9]; [1]]
let r3 = [[2;10]; [4;13]; [3]; [11;7]; [5;8]; [6;12]; [9]; [1]]
let r4 = [[12;11;6;4;1]; [8;7;13]; [5]; [3]; [2;10]; [9]]


[<EntryPoint>]
let main _ = 
    let rankings = [r1;r2;r3;r4]
    let output = new System.IO.StringWriter() 
    
    output.WriteLine "== Rankings =="
    List.iter (sprintf "%A" >> output.WriteLine) rankings

    output.WriteLine ""

    let res = List.map (fun _ -> async { return simulatedAnnealing rankings 30000 }) [0..100]
              |> Async.Parallel
              |> Async.RunSynchronously
              |> List.ofArray
              |> List.map (fun r -> kendallTauDistanceMultiStats r rankings)
              |> sortKendallTauDistanceStats
              |> List.rev
    
    output.WriteLine "== Simulated Annealing =="
    List.iter (fun r -> sprintf "%A" r.Ranking |> output.WriteLine) res
    printKendallTauDistanceStats (List.last res) |> output.WriteLine
    
    output.WriteLine ""

    let consensus = [[3]; [5]; [2;10]; [6;11;4]; [13]; [7]; [8]; [12]; [9]; [1]]
    output.WriteLine "== Consensus =="
    sprintf "%A" consensus |> output.WriteLine
    printKendallTauDistanceStats (kendallTauDistanceMultiStats consensus rankings)
    |> output.WriteLine
    
    System.IO.File.WriteAllText 
        (sprintf "simulated-annealing-rankings-%s.log" 
                 (System.DateTime.Now.ToString "yyyy-MM-dd-hh-mm-ss")
         , output.ToString ())

    0