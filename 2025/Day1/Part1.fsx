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

directions
|> Array.fold
    (fun (dial, count) rotation ->
        let newDial = rotate dial rotation
        let count = if newDial = 0 then count + 1 else count

        newDial, count)
    (50, 0)
|> snd
