open System.IO

let input = File.ReadAllLines("2025/Day7/input.txt")

let gridHeight = input.Length
let gridWidth = input[0].Length

let startingBeam = input[0].IndexOf('S')

let getNextPositions x y count =
    match input[y][x] with
    | '^' ->
        [ if x > 0 then
              x - 1, count
          if x < gridWidth - 1 then
              x + 1, count ]
    | _ -> [ (x, count) ]

let propagateRow beams y =
    beams
    |> List.collect (fun (x, count) -> getNextPositions x (y + 1) count)
    |> List.groupBy fst
    |> List.map (fun (x, counts) -> x, counts |> List.sumBy snd)

[ 0 .. gridHeight - 2 ]
|> List.fold propagateRow [ startingBeam, 1L ]
|> List.sumBy snd
