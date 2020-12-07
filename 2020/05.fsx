#! /usr/bin/env fsharpi

let lines = System.IO.File.ReadLines("05.input")

let rec binary (chars:char list) (one:char) =
  match chars with
  | [] -> 0
  | head::tail -> (if head = one then pown 2 (chars.Length-1) else 0) + (binary tail one)

let getRow (seat:char list) = binary seat 'B'

let getCol (seat:char list) = binary seat 'R'

let seatIds =
  lines
  |> Seq.toList
  |> List.map (fun line -> (Seq.toList line.[0..6], Seq.toList line.[7..]))
  |> List.map (fun (row, col) -> getRow row * 8 + getCol col)

let maxSeatId = seatIds |> List.max
let minSeatId = seatIds |> List.min
let missingSeatId = (List.sum [minSeatId..maxSeatId]) - (List.sum seatIds)

printfn "test row: %i" (getRow ['F';'B';'F';'B';'B';'F';'F'])
printfn "test col: %i" (getCol ['R';'L';'R'])
printfn "max seat: %i" maxSeatId
printfn "min seat: %i" minSeatId
printfn "missing:  %i" missingSeatId
