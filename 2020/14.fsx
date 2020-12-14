#! /usr/bin/env fsharpi

open System
open System.Text.RegularExpressions

type Mask = {And:uint64; Or:uint64}
type Mem = {Address:uint64; Value:uint64}
type Instruction = InstrMask of Mask | InstrMem of Mem
let parse (line:string): Instruction =
  let maskRegex = @"^mask = (?<value>[10X]+)$"
  let memRegex = @"^mem\[(?<address>\d+)\] = (?<value>\d+)$"
  match line with
  | x when Regex.Match(x, maskRegex).Success ->
    let value = Regex.Match(line, maskRegex).Groups.["value"].Value
    printfn "mask %s" value
    let orValue = Convert.ToUInt64(value.Replace('X', '0'), 2)
    let andValue = Convert.ToUInt64(value.Replace('X', '1'), 2)
    InstrMask {And=andValue; Or=orValue}
  | y when Regex.Match(y, memRegex).Success ->
    let groups = Regex.Match(line, memRegex).Groups
    printfn "mem %s %s" groups.["address"].Value groups.["value"].Value
    InstrMem {Address=(uint64 groups.["address"].Value); Value=(uint64 groups.["value"].Value)}
  | _ -> raise (new System.Exception("Cannot parse line"))

let instructions = System.IO.File.ReadLines("14.input") |> Seq.map parse

let applyMask (mask:Mask) (mem:Mem): uint64 = mem.Value &&& mask.And ||| mask.Or

let part1 (instructions:seq<Instruction>): uint64 =
  instructions
  |> Seq.tail
  |> Seq.fold 
    (fun (mask:Mask, memoryMap:Map<uint64,uint64>) (instruction:Instruction) -> 
      match instruction with
      | InstrMem(mem) -> (mask, memoryMap.Add(mem.Address, applyMask mask mem)) 
      | InstrMask(mask) -> (mask, memoryMap))
    ({And=uint64 0; Or=uint64 0}, Map.empty)
  |> snd
  |> Map.toSeq
  |> Seq.map snd
  |> Seq.reduce (+)

printfn "Part 1: %i" (part1 instructions)
