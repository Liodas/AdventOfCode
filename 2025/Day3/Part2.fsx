open System.IO
open System

type Battery = int

type Bank = Battery array

let toKeep = 12
let toRemove = 88

let banks: Bank array =
    File.ReadAllLines("2025/Day3/input.txt")
    |> Array.map (fun line ->
        let trimmedLine = line.Trim()

        [| 0 .. trimmedLine.Length - 1 |]
        |> Array.map (fun i -> trimmedLine[i] |> string |> Int32.Parse)
        |> fun batteries -> batteries)

let rec removeSmaller battery stk rem =
    if
        rem < toRemove
        && stk <> Array.empty
        && Array.head stk < battery
    then
        removeSmaller battery (Array.tail stk) (rem + 1)
    else
        stk, rem

let selectJoltage (bank: Bank) =
    let orderedBank =
        bank
        |> Array.fold
            (fun (stack, removed) battery ->
                let newStack, newRemoved = removeSmaller battery stack removed
                Array.append [| battery |] newStack, newRemoved)
            (Array.empty, 0)
        |> fst

    let optimizedJoltage = orderedBank |> Array.rev |> Array.take toKeep

    optimizedJoltage
    |> Array.map string
    |> String.concat ""
    |> Int64.Parse

banks |> Array.map selectJoltage |> Array.sum
