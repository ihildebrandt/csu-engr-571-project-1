module Stats

open System

type RankedValue = {
    Value: float;
    Group: int;
    Rank: float;
}

let summation list = 
    let rec sum list = 
        match list with
        | head :: tail -> head + sum tail
        | [] -> 0.0
    sum list

let product list = 
    let rec prod list =
        match list with
        | head :: tail -> head * prod tail
        | [] -> 0.0
    prod list

let mean list =
    summation list / float list.Length

let sqdiff list = 
    let mean = mean list
    list |> List.map (fun n -> (n - mean) * (n - mean))

let variance list = 
    let sqdiff = sqdiff list
    let numerator = summation sqdiff
    let denominator = float (list.Length - 1)
    numerator / denominator

let stddev list = 
    sqrt(variance list)

let zscore point list = 
    let mn = mean list
    let sd = stddev list
    let n = float list.Length

    (point - mn)/(sd/(sqrt n))

let tscore sampleA sampleB =
    let mnA = float(mean sampleA)
    let mnB = float(mean sampleB)
    let sdA = float(stddev sampleA)
    let sdB = float(stddev sampleB)
    let nA = float(sampleA.Length)
    let nB = float(sampleB.Length)

    let numerator = mnA - mnB
    let denominator = 
        sqrt(((nA-1.0)*sdA*sdA + (nB-1.0)*sdB*sdB)/(nA + nB - 1.0 - 1.0)) *
        sqrt((1.0/nA) + (1.0/nB))

    numerator/denominator

let rankSum param1 param2 =
    let assignMeanRank values =
        let ranks = List.map (fun v -> v.Rank) values
        let meanRank = mean ranks
        List.map (fun v -> { Value = v.Value; Group = v.Group; Rank = meanRank }) values

    let rec rankValues values equalValues rank = 
        match values with
        | [] -> []
        | a :: [] -> 
            [{ Value = a.Value; Group = a.Group; Rank = rank}]
        | a :: b :: rest -> 
            if a.Value = b.Value then 
                (rankValues ([b] @ rest) (equalValues @ [{ Value = a.Value; Group = a.Group; Rank = rank }]) (rank + 1.0))
            else
                (assignMeanRank (equalValues @ [{ Value = a.Value; Group = a.Group; Rank = rank }])) @
                (rankValues ([b] @ rest) [] (rank + 1.0))

    let rparam1 = [for v in param1 do yield { Value = v; Group = 1; Rank = 0.0; }]
    let rparam2 = [for v in param2 do yield { Value = v; Group = 2; Rank = 0.0; }]
    let scombined = List.sortBy (fun v -> v.Value) (rparam1 @ rparam2)
    let rcombined = rankValues scombined [] 1.0
        
    let rparam1 = Seq.toList (seq { for v in rcombined do if v.Group = 1 then yield v.Rank })
    let rsum1 = summation rparam1
            
    let rparam2 = Seq.toList (seq { for v in rcombined do if v.Group = 2 then yield v.Rank })
    let rsum2 = summation rparam2

    (rsum1, rsum2)