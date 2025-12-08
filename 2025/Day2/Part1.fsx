open System.IO

type Range =
    { Start: int64
      End: int64 }

let ranges =
    File.ReadAllText("2025/Day2/input.txt")
    |> fun s -> s.Split(',')
    |> Array.map (fun range ->
        range.Split('-')
        |> fun ids ->
            { Start = int64 ids[0]
              End = int64 ids[1] })

let findAndSumInvalidIds (range: Range) : int64 =
    [ range.Start .. range.End ]
    |> Seq.map string
    |> Seq.filter (fun s ->
        let half = s.Length / 2
        s.Substring(0, half) = s.Substring(half))
    |> Seq.map int64
    |> Seq.sum

ranges
|> Array.map findAndSumInvalidIds
|> Array.sum
