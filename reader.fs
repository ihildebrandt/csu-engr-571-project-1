module Reader

open CsvHelper
open System.IO
open CsvHelper.Configuration
open System.Globalization

let read config path mapCsv =
    seq {
        use reader = new StreamReader(path = path)
        use csv = new CsvReader(reader, configuration = config)

        csv.Read() |> ignore
        csv.ReadHeader() |> ignore

        while csv.Read() do
            yield mapCsv csv
    }

let readCsv path mapCsv =
    let config = new CsvConfiguration(CultureInfo.CurrentCulture)
    config.Delimiter <- ","
    read config path mapCsv