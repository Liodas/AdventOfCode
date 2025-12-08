open System.IO

type Coords =
    { X: int
      Y: int
      Z: int }

type UnionFind =
    { Parent: Map<int, int>
      Rank: Map<int, int> }

let boxesCoords =
    File.ReadAllLines("2025/Day8/input.txt")
    |> Array.map (fun line ->
        let coords = line.Split(',')

        { X = int coords[0]
          Y = int coords[1]
          Z = int coords[2] })

let distanceSquared (a: Coords) (b: Coords) =
    let dx = int64 (a.X - b.X)
    let dy = int64 (a.Y - b.Y)
    let dz = int64 (a.Z - b.Z)

    dx * dx + dy * dy + dz * dz

let generatePairs (boxes: Coords array) =
    let boxesCount = boxes.Length

    [ 0 .. boxesCount - 2 ]
    |> List.collect (fun i ->
        [ i + 1 .. boxesCount - 1 ]
        |> List.map (fun j ->
            let dist = distanceSquared boxes[i] boxes[j]
            dist, i, j))

let initUnionFind count =
    { Parent =
        [ 0 .. count - 1 ]
        |> List.map (fun i -> i, i)
        |> Map.ofList
      Rank =
        [ 0 .. count - 1 ]
        |> List.map (fun i -> i, 0)
        |> Map.ofList }

let rec find (uf: UnionFind) x =
    let parent = uf.Parent[x]

    if parent <> x then
        let root, uf1 = find uf parent

        root,
        { uf1 with
            Parent = uf1.Parent.Add(x, root) }
    else
        x, uf

let union (uf: UnionFind) x y =
    let rootX, uf1 = find uf x
    let rootY, uf2 = find uf1 y

    if rootX <> rootY then
        let rankX = uf2.Rank[rootX]
        let rankY = uf2.Rank[rootY]

        if rankX < rankY then
            { uf2 with
                Parent = uf2.Parent.Add(rootX, rootY) }
        elif rankX > rankY then
            { uf2 with
                Parent = uf2.Parent.Add(rootY, rootX) }
        else
            { Parent = uf2.Parent.Add(rootY, rootX)
              Rank = uf2.Rank.Add(rootX, rankX + 1) }
    else
        uf2

let pairs =
    generatePairs boxesCoords
    |> List.sortBy (fun (dist, _, _) -> dist)
    |> List.take 1000

let boxesCount = boxesCoords.Length

let finalUnionFind =
    pairs
    |> List.fold (fun uf (_, i, j) -> union uf i j) (initUnionFind boxesCount)

let getCircuitLengths (uf: UnionFind) =
    [ 0 .. boxesCount - 1 ]
    |> List.map (fun i -> fst (find uf i))
    |> List.groupBy id
    |> List.map (fun (_, group) -> List.length group)

getCircuitLengths finalUnionFind
|> List.sortDescending
|> List.take 3
|> fun largestCircuits ->
    largestCircuits[0]
    * largestCircuits[1]
    * largestCircuits[2]
