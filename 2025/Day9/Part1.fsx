open System.IO

type Coords =
    { X: int64
      Y: int64 }

let redTilesCoords =
    File.ReadAllLines("2025/Day9/input.txt")
    |> Array.map (fun line ->
        let coords = line.Split(',')

        { X = int64 coords[0]
          Y = int64 coords[1] })

let calculateArea (c1: Coords) (c2: Coords) =
    (abs (c1.X - c2.X) + 1L)
    * (abs (c1.Y - c2.Y) + 1L)

let findLargestRectangle (redTilesCoords: Coords array) =
    redTilesCoords
    |> Array.mapi (fun i tile -> i, tile)
    |> Array.collect (fun (i, tile1) ->
        redTilesCoords
        |> Array.skip (i + 1)
        |> Array.map (fun tile2 -> tile1, tile2))
    |> Array.map (fun (tile1, tile2) -> calculateArea tile1 tile2)
    |> Array.max

findLargestRectangle redTilesCoords
