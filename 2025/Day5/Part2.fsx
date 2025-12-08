open System.IO
open System

type Range =
    { Start: int64
      End: int64 }

let sections =
    File.ReadAllText("2025/Day5/input.txt").Split([| "\r\n\r\n" |], StringSplitOptions.RemoveEmptyEntries)

let ids =
    sections[0].Split([| "\r\n" |], StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun line ->
        let parts = line.Split('-')

        { Start = int64 parts[0]
          End = int64 parts[1] })

ids
|> Array.sortBy _.Start
|> Array.fold
    (fun (merged: Range list) range ->
        match merged with
        | [] -> [ range ]
        | head :: tail ->
            if range.Start <= head.End + 1L then
                { Start = head.Start
                  End = max head.End range.End }
                :: tail
            else
                range :: merged)
    []
|> List.sumBy (fun range -> range.End - range.Start + 1L)
