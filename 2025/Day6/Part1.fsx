open System.IO

type Operation =
    | Addition
    | Multiplication

let input = File.ReadAllLines("2025/Day6/input.txt")

let symbols =
    input[input.Length - 1]
    |> Seq.filter (fun symbol -> symbol = '*' || symbol = '+')
    |> Seq.toList

let numbers =
    input[0 .. input.Length - 2]
    |> Array.map (fun line ->
        line.Split(' ', System.StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun number -> number.Trim()))

let getOperationType symbol =
    match symbol with
    | '+' -> Addition
    | '*' -> Multiplication
    | _ -> failwith "Unsupported operation"

[ 0 .. numbers[0].Length - 1 ]
|> List.map (fun i ->
    let columnValues =
        numbers
        |> Array.map (fun number -> number[i])
        |> Array.map int64

    match getOperationType symbols[i] with
    | Addition -> Array.sum columnValues
    | Multiplication -> Array.reduce (fun nb1 nb2 -> nb1 * nb2) columnValues)
|> List.sum
