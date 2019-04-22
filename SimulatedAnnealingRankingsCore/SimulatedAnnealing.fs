module SimulatedAnnealing
open Random
open KendallTauDistance

let move ranking =
    let clusterRank = random.Next (List.length ranking)
    let newRank = clusterRank + (if flip () then 1 else -1)
    let cluster = List.item clusterRank ranking
    let element = pick cluster
    if List.length cluster > 1
    then let filteredCluster = List.filter ((<>) element) cluster
         ranking
         |> List.collect (fun c -> if c = cluster
                                   then if newRank < clusterRank
                                        then [[element]; filteredCluster]
                                        else [filteredCluster; [element]]
                                   else [c])
    elif newRank < 0 || newRank >= List.length ranking
    then ranking
    else ranking
         |> List.mapi (fun rank c -> if c = cluster then []
                                     elif rank = newRank then element :: c
                                     else c)
         |> List.filter (Seq.isEmpty >> not)

let simulatedAnnealing rankings steps =
    let initialTemp = 60.0
    let initialRanking = pick rankings

    let temperature step = (1.0 - float step / float steps * 1.2) * initialTemp
                           |> max 0.0
    let accept current neighbour temperature =
        if neighbour < current then true
        else random.NextDouble () * temperature > float (neighbour - current)
    List.fold (fun (current, currentScore) i ->
                let neighbour = move current
                let neighbourScore = kendallTauDistanceMulti neighbour rankings
                let temp = temperature i
                if accept currentScore neighbourScore temp
                then neighbour, neighbourScore
                else current, currentScore)
              (initialRanking, kendallTauDistanceMulti initialRanking rankings)
              [0..steps]
    |> fst
