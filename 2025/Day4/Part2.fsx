open System.IO

let readInput () =
    File.ReadAllLines("2025/Day4/input.txt")
    |> Array.map (fun line -> line.Trim())

let mutable lines = readInput ()

let countSurroundingRolls x y =
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

let goThroughGrid () =
    lines
    |> Array.mapi (fun yIndex line ->
        line.ToCharArray()
        |> Array.mapi (fun xIndex char ->
            match char with
            | '@' ->
                match countSurroundingRolls xIndex yIndex with
                | rollCount when rollCount < 4 -> 'X'
                | _ -> '@'
            | _ -> '.')
        |> System.String)

let countRemovedRolls () =
    lines
    |> Array.map (fun line ->
        line.ToCharArray()
        |> Array.filter (fun char -> char = 'X')
        |> Array.length)
    |> Array.sum

let rec loopUntilCleaned () =
    let newLines = goThroughGrid ()

    if newLines = lines then
        countRemovedRolls ()
    else
        lines <- newLines
        loopUntilCleaned ()

loopUntilCleaned ()
