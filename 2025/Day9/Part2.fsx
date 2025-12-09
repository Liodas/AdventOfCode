open System.IO

type Coords =
    { X: int64
      Y: int64 }

let redTilesCoords =
    File.ReadAllLines("2025/day9/input.txt")
    |> Array.map (fun line ->
        let parts = line.Split(',') |> Array.map int64

        { X = parts[0]
          Y = parts[1] })

let perimeter =
    let pairs = Array.pairwise redTilesCoords

    let segments = Array.map (fun (t1, t2) -> t1, t2) pairs

    let lastSegment = redTilesCoords.[redTilesCoords.Length - 1], redTilesCoords.[0]
    Array.append segments [| lastSegment |]

let calculateArea (c1: Coords, c2: Coords) =
    (abs (c1.X - c2.X) + 1L)
    * (abs (c1.Y - c2.Y) + 1L)

let rectangles =
    [| 0 .. redTilesCoords.Length - 2 |]
    |> Array.collect (fun i ->
        [| i + 1 .. redTilesCoords.Length - 1 |]
        |> Array.map (fun j -> redTilesCoords.[i], redTilesCoords.[j]))
    |> Array.sortByDescending calculateArea

let intersects (t1: Coords, t2: Coords) (s1: Coords, s2: Coords) =
    let minX1, maxX1 = min t1.X t2.X, max t1.X t2.X
    let minY1, maxY1 = min t1.Y t2.Y, max t1.Y t2.Y
    let minX2, maxX2 = min s1.X s2.X, max s1.X s2.X
    let minY2, maxY2 = min s1.Y s2.Y, max s1.Y s2.Y

    not (
        maxX1 <= minX2
        || maxX2 <= minX1
        || maxY1 <= minY2
        || maxY2 <= minY1
    )

let largestRectangle =
    rectangles
    |> Array.find (fun rectangle ->
        perimeter
        |> Array.forall (fun segment -> not (intersects rectangle segment)))

calculateArea largestRectangle
