open System.IO

let input = File.ReadAllLines("2025/Day7/input.txt")

let startingBeam = input[0].IndexOf('S')

let beams = Set.singleton startingBeam

input
|> Array.skip 1
|> Array.fold
    (fun (beams, hits) line ->
        let newBeams, newHits =
            beams
            |> Set.fold
                (fun (acc, hitAcc) beam ->
                    match line[beam] with
                    | '^' -> Set.union (Set.ofList [ beam - 1; beam + 1 ]) acc, hitAcc + 1
                    | _ -> Set.add beam acc, hitAcc)
                (Set.empty, 0)

        newBeams, hits + newHits)
    (beams, 0)
|> snd
