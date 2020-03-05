open System;
open Reader;
open Stats;

type Record = {
    Year: int;
    Week: int;
    Deleted: float;
    Sent: float;
    Total: float;
    Description: string;
}

let getDeleted =
    fun data -> seq { for p in data do yield p.Deleted }
    
let getSent = 
    fun data -> seq { for p in data do yield p.Sent }
        
let getTotal = 
    fun data -> seq { for p in data do yield p.Total }

let queryForYear year =
    fun data -> seq { for p in data do if p.Year = year then yield p}

let queryForDescription description =
    fun data -> seq { for p in data do if p.Description = description then yield p }

let readCsvYear year = 
    let mapCsv year = 
        (fun (csv:CsvHelper.CsvReader) -> {
            Year = year;
            Week = (csv.GetField("Week") |> int);
            Deleted = (csv.GetField("Deleted") |> float);
            Sent = (csv.GetField("Sent") |> float);
            Total = (csv.GetField("Total") |> float);
            Description = csv.GetField("Description");
        })

    let file = sprintf "%i_simplified.csv" year
    Seq.toList (Reader.readCsv file (mapCsv year))

let years = [2011;2012;2013;2014;2015;2016;2017]
let descriptions = ["Normal";"Travel";"Holiday";"Vacation";"Announcement"]

let rec buildData years = 
    match years with 
    | year :: rest -> (readCsvYear year) @ (buildData rest)
    | [] -> []

//let buildData years = 
//    let mutable data = []
//    for year in years do
//        data <- List.append data (readCsvYear year)
//    data

let data = buildData years

let p1() = 
    let calculateStats parameterQuery =
        let parameter = Seq.toList parameterQuery
        let mn = Stats.mean parameter
        let sd = Stats.stddev parameter
        printf "%f\t%f" mn sd
        printfn ""

    printfn "Year\tDescription\tParameter\tMean\tStdDev"
    for year in years do
        printf "%i\t\tDeleted\t" year
        data |> queryForYear year |> getDeleted |> calculateStats
        printf "%i\t\tSent\t" year
        data |> queryForYear year |> getSent |> calculateStats
        printf "%i\t\tTotal\t" year
        data |> queryForYear year |> getTotal |> calculateStats

    printf "\t\tDeleted\t"
    data |> getDeleted |> calculateStats
    printf "\t\tSent\t"
    data |> getSent |> calculateStats
    printf "\t\tTotal\t"
    data |> getTotal |> calculateStats

    for description in descriptions do
        printf "\t%s\tDeleted\t" description
        data |> queryForDescription description |> getDeleted |> calculateStats
        printf "\t%s\tSent\t" description
        data |> queryForDescription description |> getSent |> calculateStats
        printf "\t%s\tTotal\t" description
        data |> queryForDescription description |> getTotal |> calculateStats


let p2() = 
    let calculateStats a b f = 
        let _a = a |> f |> Seq.toList
        let _b = b |> f |> Seq.toList
        let score = Stats.tscore _a _b 
        printfn "%f" score

    let compare description others = 
        let a = data |> queryForDescription description
        for other in others do
            let b = data |> queryForDescription other
            for t in ["Deleted";"Sent";"Total"] do
                printf "%s\t%s\t%s\t" description other t 
                match t with 
                    | "Deleted" -> (calculateStats a b getDeleted) |> ignore
                    | "Sent" -> (calculateStats a b getSent) |> ignore
                    | "Total" -> (calculateStats a b getTotal) |> ignore
                    | _ -> ()

    printfn "FirstDescription\tSecondDescription\tParameter\tTTestScore"

    let rec compareAll descs =
        match descs with 
            | head :: tail -> (compare head tail); compareAll tail; ()
            | [] -> ()

    compareAll descriptions

let p3() =
    let totalSent = Stats.summation (data |> getSent |> Seq.toList)
    let totalTotal = Stats.summation (data |> getTotal |> Seq.toList)
    let totalRatio = totalSent / totalTotal
 
    let rec countRatios list =
        match list with
            | head :: tail -> (if head.Sent / head.Total > totalRatio then 1 else 0) + countRatios tail
            | [] -> 0

    let travelData = data |> queryForDescription "Travel" |> Seq.toList

    let countAllRatios = countRatios data
    let countTravelRatios = countRatios travelData

    let P_R = ( (float countAllRatios) / (float data.Length) )
    let P_RgT = ( (float countTravelRatios) / (float data.Length) )
    let P_T = ( (float travelData.Length) / (float data.Length) )

    let ratioData = data |> (fun data -> seq { for p in data do if p.Sent / p.Total > totalRatio then yield p }) |> queryForDescription "Travel" |> Seq.toList

    printfn "Probability that the ratio for Sent/Total (R) for a week"
    printfn "is greater than the same ratio over all weeks (O)."
    printfn ""
    printfn "1.   Overall Sent/Total ratio (O): %f" totalRatio
    printfn "2.                Weeks where R>O: %i" countAllRatios
    printfn "3.                         P(R>O): %i / %i = %f" countAllRatios data.Length P_R
    printfn "4.         Travel weeks where R>O: %i" countTravelRatios
    printfn "5.                       P(R>O|T): %i / %i = %f" countTravelRatios data.Length P_RgT
    printfn "6.                           P(T): %i / %i = %f" travelData.Length data.Length P_T 
    printfn "7. P(T|R>O) = P(R>O|T)P(T)/R(R>O): %f * %f / %f = %f" P_RgT P_T P_R (P_RgT * P_T / P_R)
    printfn "8.                         Actual: %i / %i = %f" ratioData.Length data.Length ((float ratioData.Length) / (float data.Length)) 

let p4() =
    printfn "Using a T-Test and Wilcoxon Rank Sum: Are the "
    printfn "number of emails Sent during Holiday weeks "
    printfn "and Vacation weeks statistically similar? "
    printfn ""

    let pHoliday = data |> queryForDescription "Holiday" |> getSent |> Seq.toList
    let pVacation = data |> queryForDescription "Vacation" |> getSent |> Seq.toList

    let tscore = Stats.tscore pHoliday pVacation
    let (rHoliday, rVacation) = Stats.rankSum pHoliday pVacation

    printfn "T-Score: %f" tscore 
    printfn "Rank-Sum: "
    printfn "     Holiday: %i %f" pHoliday.Length rHoliday
    printfn "    Vacation: %i %f" pVacation.Length rVacation
    printfn ""

let runProblem problemNumber =
    match problemNumber with
    | 1 -> p1()
    | 2 -> p2()
    | 3 -> p3()
    | 4 -> p4()
    | _ -> printfn "unrecognized problem number"

[<EntryPoint>]
let main args = 
    match args |> Array.truncate 1 with
    | [| problem |] -> runProblem (int problem)
    | _ -> printf "pass a problem number"

    0
