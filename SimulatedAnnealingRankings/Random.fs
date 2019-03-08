module Random

let random = System.Random()
let flip () = random.Next 2 = 0
let shuffle list = List.map (fun e -> random.NextDouble (), e) list
                   |> List.sortBy fst
                   |> List.map snd
let pickMany n list = List.take n (shuffle list)
let pick list = pickMany 1 list |> List.head