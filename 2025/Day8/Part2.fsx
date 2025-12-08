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

let countComponents (uf: UnionFind) =
    [ 0 .. boxesCoords.Length - 1 ]
    |> List.map (fun i -> fst (find uf i))
    |> List.distinct
    |> List.length

let pairs =
    generatePairs boxesCoords
    |> List.sortBy (fun (dist, _, _) -> dist)

let rec processUntilOneComponent uf remainingPairs =
    match remainingPairs with
    | (_, i, j) :: rest ->
        let rootI, uf1 = find uf i
        let rootJ, uf2 = find uf1 j

        if rootI <> rootJ then
            let newUF = union uf2 i j

            if countComponents newUF = 1 then
                i, j
            else
                processUntilOneComponent newUF rest
        else
            processUntilOneComponent uf2 rest
    | [] -> failwith "No more pairs to process"

let lastBoxI, lastBoxJ =
    processUntilOneComponent (initUnionFind boxesCoords.Length) pairs

boxesCoords[lastBoxI].X * boxesCoords[lastBoxJ].X
