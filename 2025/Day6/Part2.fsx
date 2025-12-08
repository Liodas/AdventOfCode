open System.IO
open System.Text.RegularExpressions

let parsedInput =
    File.ReadAllLines("2025/Day6/input.txt")
    |> Array.map (fun line -> Seq.toList line)
    |> fun lines ->
        let length = lines[0].Length

        [ length - 1 .. -1 .. 0 ]
        |> List.map (fun col -> lines |> Array.map (fun line -> line[col]))
        |> Array.concat
        |> Array.map string
        |> String.concat ""
        |> fun str -> Regex.Split(str, @"([*+])")

let symbols =
    parsedInput
    |> Array.filter (fun s -> s = "+" || s = "*")

let numbers =
    parsedInput
    |> Array.filter (fun s -> s <> "+" && s <> "*")
    |> Array.map (fun col ->
        col.Split(' ', System.StringSplitOptions.RemoveEmptyEntries)
        |> Array.map int64)
    |> Array.filter (fun arr -> arr.Length > 0)

numbers
|> Array.mapi (fun i nums ->
    nums
    |> Array.fold
        (fun acc n ->
            match symbols[i] with
            | "+" -> acc + n
            | "*" -> if acc = 0L then acc + n else acc * n
            | _ -> acc)
        0L)
|> Array.sum
