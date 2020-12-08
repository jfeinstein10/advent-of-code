#! /usr/bin/env fsharpi

open System

let lines = 
  System.IO.File.ReadLines("08.input")
  |> Seq.toList

let rec execute (lines:string list) (currentLine:int) (acc:int) (visited:Set<int>):(int * int) = 
  if visited.Contains(currentLine) || currentLine >= lines.Length
  then 
    (acc, currentLine)
  else
    let command = lines.[currentLine].[0..2]
    let amount = int lines.[currentLine].[3..]
    match command with
    | "acc" -> execute lines (currentLine + 1) (acc + amount) (visited.Add(currentLine))
    | "jmp" -> execute lines (currentLine + amount) acc (visited.Add(currentLine))
    | "nop" -> execute lines (currentLine + 1) acc (visited.Add(currentLine))
    | _ -> (acc, currentLine)

let part1 (lines:string list):int = 
  let (acc, _) = execute lines 0 0 Set.empty
  acc

printfn "Part 1: %i" (part1 lines)

let part2 (lines:string list):int =
  let swapInstruction (line:string) = 
    if line.StartsWith "jmp" then line.Replace("jmp", "nop") else line.Replace("nop", "jmp")
  let replace list line = 
    list |> List.mapi (fun idx value -> if idx = line then swapInstruction value else value)
  let rec swapAndRun (lines:string list) (idx:int) =
    if idx >= lines.Length
    then
      0
    else
      let newLines = replace lines idx
      let (acc, endingLine) = execute newLines 0 0 Set.empty
      if endingLine >= newLines.Length then acc else swapAndRun lines (idx+1)
  swapAndRun lines 0

printfn "Part 2: %i" (part2 lines)