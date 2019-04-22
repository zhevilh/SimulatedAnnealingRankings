module KendallTauDistance

type KendallTauDistanceStats = { Ranking: int list list
                                 Min: int
                                 Max: int
                                 Mean: float
                                 Median: float }

let findElementRank element ranking =
    List.findIndex (List.contains element) ranking
    
let kendallTauDistance r1 r2 =
    let combinations2 list =
        let a = Array.ofList list
        List.mapi (fun index e ->
                    [for i = index + 1 to (Array.length a) - 1 do
                        yield (e, a.[i])])
                  list
        |> List.collect id

    let elements = List.collect id r1
    let rankedElements = List.map (fun e -> findElementRank e r1, findElementRank e r2) elements

    let inline compare rank1 rank2 = if rank1 = rank2 then 0
                                     elif rank1 < rank2 then -1
                                     else 1

    combinations2 rankedElements
    |> List.map (fun ((e1r1, e1r2), (e2r1,e2r2)) -> if compare e1r1 e2r1 = compare e1r2 e2r2 then 0 else 1)
    |> List.reduce (+)

let kendallTauDistanceMulti r rs =
    List.map (kendallTauDistance r) rs
    |> List.reduce (+)

let kendallTauDistanceMultiStats r rs =
    let distances = List.map (kendallTauDistance r) rs
                    |> List.sort
    { Ranking = r
      Min = List.head distances
      Max = List.last distances
      Mean = float (List.reduce (+) distances) / float (List.length distances)
      Median = let n = List.length distances 
               let middle = n / 2
               if n % 2 = 0
               then (float distances.[middle] + float distances.[middle - 1]) / 2.0
               else float distances.[middle] }

let sortKendallTauDistanceStats =
    List.sortWith (fun s1 s2 ->
                     if s1.Mean = s2.Mean
                     then if s1.Median = s2.Median
                          then if s1.Min = s2.Min
                               then s1.Max.CompareTo s2.Max
                               else s1.Min.CompareTo s2.Min
                          else s1.Median.CompareTo s2.Median
                     else s1.Mean.CompareTo s2.Mean)

let printKendallTauDistanceStats stats =
    sprintf "Min: %i\nMax: %i\nMean: %f\nMedian: %f"
            stats.Min stats.Max stats.Mean stats.Median