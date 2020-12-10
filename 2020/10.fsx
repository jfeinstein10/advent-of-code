#! /usr/bin/env fsharpi

open System.Collections.Generic

let lines = 
  System.IO.File.ReadLines("10.input")
  |> Seq.toList
  |> List.map (int)
  |> List.sort

let part1 lines =
  let differences = lines |> List.mapi (fun idx value -> if idx = 0 then value else value - lines.[idx-1])
  let differences1 = differences |> List.filter ((=) 1) |> List.length
  let differences3 = differences |> List.filter ((=) 3) |> List.length
  differences1 * (differences3 + 1)
  
printfn "Part 1: %i" (part1 lines)

let part2 (lines:int list):float = 
  let mutable memoize = Map.empty
  let lastValue = List.max lines
  let rec getPaths (lines:int list) (value:int):float =
    if memoize.ContainsKey(value)
    then memoize.[value]
    else
      if value = lastValue
      then 1.0
      else
        let numPaths = 
          [1..3] 
          |> List.map (fun stepToNextValue -> 
            if List.contains (value + stepToNextValue) lines 
            then getPaths lines (value + stepToNextValue)
            else 0.0)
          |> List.sum
        memoize <- memoize.Add(value, numPaths)
        numPaths
  getPaths lines 0

printfn "Part 2: %f" (part2 lines)