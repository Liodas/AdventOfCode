open System.IO
open System

type Battery = int

type Bank = Battery array

let banks: Bank array =
    File.ReadAllLines("2025/Day3/input.txt")
    |> Array.map (fun line ->
        let trimmedLine = line.Trim()

        [| 0 .. trimmedLine.Length - 1 |]
        |> Array.map (fun i -> trimmedLine[i] |> string |> Int32.Parse)
        |> fun batteries -> batteries)

banks
|> Array.map (fun bank ->
    let firstBatteryIndex, firstBattery =
        bank
        |> Array.mapi (fun index battery -> index, battery)
        |> Array.filter (fun (i, _) -> i < bank.Length - 1)
        |> Array.maxBy snd

    let secondBattery =
        bank
        |> Array.mapi (fun index battery -> index, battery)
        |> Array.filter (fun (i, _) -> i > firstBatteryIndex)
        |> Array.maxBy snd
        |> snd

    firstBattery * 10 + secondBattery)
|> Array.sum
