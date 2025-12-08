open System.IO
open System

type Range =
    { Start: int64
      End: int64 }

let sections =
    File.ReadAllText("2025/Day5/input.txt").Split([| "\r\n\r\n" |], StringSplitOptions.RemoveEmptyEntries)

let freshIds =
    sections[0].Split([| "\r\n" |], StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun line ->
        let parts = line.Split('-')

        { Start = int64 parts[0]
          End = int64 parts[1] })

let ingredientIds =
    sections[1].Split([| "\r\n" |], StringSplitOptions.RemoveEmptyEntries)
    |> Array.map int64

let isIngredientFresh (ingredientId: int64) (ranges: Range array) =
    ranges
    |> Array.exists (fun range ->
        ingredientId >= range.Start
        && ingredientId <= range.End)

ingredientIds
|> Array.filter (fun ingredientId -> isIngredientFresh ingredientId freshIds)
|> Array.length
