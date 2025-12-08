open System.IO

let lines: string array =
    File.ReadAllLines("2025/Day4/input.txt")
    |> Array.map (fun line -> line.Trim())

let verifyPaperRollsAround x y =
    let surroundingSlots =
        [| x - 1, y - 1
           x, y - 1
           x + 1, y - 1
           x - 1, y
           x + 1, y
           x - 1, y + 1
           x, y + 1
           x + 1, y + 1 |]

    surroundingSlots
    |> Array.filter (fun (x, y) ->
        y >= 0
        && y < lines.Length
        && x >= 0
        && x < lines[y].Length)
    |> Array.map (fun (x, y) -> lines[y][x])
    |> Array.filter (fun char -> char = '@')
    |> Array.length

lines
|> Array.mapi (fun yIndex line ->
    line.ToCharArray()
    |> Array.mapi (fun xIndex char ->
        match char with
        | '@' ->
            match verifyPaperRollsAround xIndex yIndex with
            | rollCount when rollCount < 4 -> 1
            | _ -> 0
        | _ -> 0))
|> Array.concat
|> Array.sum
