#! /usr/bin/env fsharpi

open System

let lines = 
  System.IO.File.ReadLines("09.input")
  |> Seq.toList
  |> List.map (float)

let part1 (lines:float list) =
  let generatePairs (list:float list):Set<float> =
    list.[..list.Length - 1]
    |> List.mapi (fun idx value -> list.[idx+1..] |> List.map (fun secondValue -> value + secondValue)) 
    |> List.concat
    |> Set.ofList
  lines.[25..]
  |> List.mapi (fun idx value -> (value, (generatePairs lines.[idx..idx+24]).Contains(value)))
  |> List.filter (fun tuple -> not (snd tuple))
  |> List.head
  |> fst

let part1Soln = part1 lines
printfn "Part 1: %f" part1Soln

let part2 (lines: float list) (target:float) =
  let rec contiguousSum (lines: float list) (target:float) (acc:float) (accList: float list) =
    if acc >= target then (acc, accList) else
    match lines with
    | [] -> (acc, accList)
    | head::tail -> contiguousSum tail target (acc+head) (head::accList)
  let sequence = 
    lines
    |> List.mapi (fun idx _ -> contiguousSum lines.[idx..] target 0.0 [])
    |> List.filter (fun tuple -> (fst tuple) = target)
    |> List.head
    |> snd
  List.min sequence + List.max sequence

printfn "Part 2: %f" (part2 lines part1Soln)