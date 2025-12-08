open System.IO

let parseLine (line: string) =
    match line[0] with
    | 'L' -> int line[1..] * -1
    | 'R' -> int line[1..]
    | _ -> failwith "Invalid input"

let directions =
    File.ReadAllLines("2025/Day1/input.txt")
    |> Array.map parseLine

let rotate dial rotation =
    let newValue = dial + rotation

    (newValue % 100 + 100) % 100

let countZeroHits (dial: int) (rotation: int) =
    match abs rotation with
    | 0 -> 0
    | nbSteps ->
        let target =
            if rotation > 0 then
                (100 - dial) % 100
            else
                dial % 100

        match target, nbSteps with
        | 0, _ -> nbSteps / 100
        | t, s when t > s -> 0
        | _ -> 1 + (nbSteps - target) / 100

directions
|> Array.fold
    (fun (dial, count) rotation ->
        let zeroHits = countZeroHits dial rotation
        let newDial = rotate dial rotation
        newDial, count + zeroHits)
    (50, 0)
|> snd
