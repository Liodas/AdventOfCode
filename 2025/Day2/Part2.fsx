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

let isInvalidId (id: string) =
    [| 1 .. (id.Length / 2) |]
    |> Array.exists (fun subIdLength ->
        if id.Length % subIdLength <> 0 then
            false
        else
            let substr = id.Substring(0, subIdLength)
            let times = id.Length / subIdLength

            let built = String.concat "" (List.init times (fun _ -> substr))
            built = id)

let findAndSumInvalidIds range : int64 =
    [ range.Start .. range.End ]
    |> Seq.map string
    |> Seq.filter isInvalidId
    |> Seq.map int64
    |> Seq.sum

ranges
|> Array.map findAndSumInvalidIds
|> Array.sum
