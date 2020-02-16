module Stats

open System

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