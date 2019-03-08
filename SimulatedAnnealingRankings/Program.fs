open KendallTauDistance
open SimulatedAnnealing

let r1 = [[4]; [2]; [1]; [3;11]; [7;14;16;5;8;9;10;13;15;6;12]]
let r2 = [[7;14;16;5;8;9;10;13;15;6;12]; [3;11]; [1]; [2]; [4]]
let r3 = [[1]; [11;2;7;16;5;8;9;10;13;15]; [4]; [3]; [14;6;12]]
let r4 = [[11;4;3;12;6;14]; [1]; [2]; [10;13;15;16;5;8;9;7]]
let r5 = [[10;13;15;16;5;8;9;7]; [1]; [2]; [11];[4];[3];[12;6;14]]
let r6 = [[12;6;14]; [3]; [4]; [11]; [2]; [1]; [10;13;15;16;5;8;9;7]]
let r7 = [[11]; [12]; [10;13;15;6]; [2]; [1;16;5;8;9]; [4]; [3]; [7;14]]
let r8 = [[7;14]; [3]; [4]; [1;16;5;8;9]; [2]; [10;13;15;6]; [12]; [11]]

[<EntryPoint>]
let main _ = 
    let rankings = [r1;r2;r3;r4;r5;r6;r7;r8]
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

    let consensus = [[11]; [4]; [1]; [2]; [3]; [14; 6; 12]; [7; 16; 5; 8; 9; 10; 13; 15]]
    output.WriteLine "== Consensus =="
    sprintf "%A" consensus |> output.WriteLine
    printKendallTauDistanceStats (kendallTauDistanceMultiStats consensus rankings)
    |> output.WriteLine
    
    System.IO.File.WriteAllText 
        (sprintf "simulated-annealing-rankings-%s.log" 
                 (System.DateTime.Now.ToString "yyyy-MM-dd-hh-mm-ss")
         , output.ToString ())

    0