#! /usr/bin/env fsharpi

let lines = System.IO.File.ReadLines("13.input") |> Seq.toArray
let startTime = int lines.[0]
let buses = lines.[1].Split(',')

let part1 (startTime:int) (buses:seq<int>): int =
  Seq.initInfinite (fun i -> i + startTime)
  |> Seq.map (fun time -> (time, buses |> Seq.filter (fun bus -> time % bus = 0)))
  |> Seq.skipWhile (fun (time, buses) -> Seq.length buses = 0)
  |> Seq.head
  |> fun (time, buses) -> (time - startTime) * (Seq.head buses)

let part1buses = buses |> Seq.filter ((<>) "x") |> Seq.map int
printfn "Part 1: %i" (part1 startTime part1buses)

let part2 (buses:seq<double*double>): double =
  let nextAcc ((accOffset,accMultiple):double*double) ((offset,bus):double*double): double*double =
    Seq.initInfinite (fun i -> accOffset + (double i) * accMultiple)
    |> Seq.skipWhile (fun newOffset -> (newOffset + offset) % bus <> 0.0)
    |> Seq.head
    |> fun newOffset -> (newOffset, accMultiple*bus)
  buses
  |> Seq.fold nextAcc (1.0, 1.0)
  |> fst

let part2buses = 
  buses 
  |> Seq.mapi (fun idx bus -> (idx, bus)) 
  |> Seq.filter (fun (_, bus) -> bus <> "x") 
  |> Seq.map (fun (idx, bus) -> (double idx, double bus)) 
  |> Seq.toList
printfn "Part 2: %f" (part2 part2buses)