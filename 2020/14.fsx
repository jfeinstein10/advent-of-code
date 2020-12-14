#! /usr/bin/env fsharpi

open System
open System.Text.RegularExpressions

type Mask = {Value:string}
type Mem = {Address:int64; AddressString:string; Value:int64; ValueString:string}
type Instruction = InstrMask of Mask | InstrMem of Mem
let parse (line:string): Instruction =
  let maskRegex = @"^mask = (?<value>[10X]+)$"
  let memRegex = @"^mem\[(?<address>\d+)\] = (?<value>\d+)$"
  match line with
  | x when Regex.Match(x, maskRegex).Success ->
    let value = Regex.Match(line, maskRegex).Groups.["value"].Value
    InstrMask {Value=value}
  | y when Regex.Match(y, memRegex).Success ->
    let groups = Regex.Match(line, memRegex).Groups
    let address = groups.["address"].Value
    let value = groups.["value"].Value
    InstrMem {Address=(int64 address); AddressString=address; Value=(int64 value); ValueString=value}
  | _ -> raise (Exception("Cannot parse line"))

let instructions = System.IO.File.ReadLines("14.input") |> Seq.map parse

let part1 (instructions:seq<Instruction>): int64 =
  let applyMask (mask:Mask) (mem:Mem): int64 =
    let orValue = Convert.ToInt64(mask.Value.Replace('X', '0'), 2)
    let andValue = Convert.ToInt64(mask.Value.Replace('X', '1'), 2)
    mem.Value &&& andValue ||| orValue
  instructions
  |> Seq.fold
    (fun (mask:Mask, memoryMap:Map<int64,int64>) (instruction:Instruction) ->
      match instruction with
      | InstrMem(mem) ->
        (mask, memoryMap.Add(mem.Address, applyMask mask mem))
      | InstrMask(newMask) -> (newMask, memoryMap))
    ({Value=""}, Map.empty)
  |> snd
  |> Map.toSeq
  |> Seq.map snd
  |> Seq.reduce (+)

printfn "Part 1: %i" (part1 instructions)

let part2 (instructions:seq<Instruction>): int64 =
  let getAddresses (mask:Mask) (mem:Mem): seq<int64> =
    mask.Value.ToCharArray()
    |> Seq.zip (Convert.ToString(mem.Address, 2).PadLeft(36, '0').ToCharArray())
    |> Seq.fold
      (fun acc (addressChar, maskChar) ->
        match maskChar with
        | '1'  -> acc |> List.map (fun address -> address + "1")
        | '0' -> acc |> List.map (fun address -> address + (string addressChar))
        | _ -> acc |> List.collect (fun address -> [address + "0"; address + "1"]))
      [""]
    |> Seq.map (fun address -> Convert.ToInt64(address, 2))
  instructions
  |> Seq.fold
    (fun (mask:Mask, memoryMap:Map<int64,int64>) (instruction:Instruction) ->
      match instruction with
      | InstrMem(mem) ->
        getAddresses mask mem
        |> Seq.fold
          (fun (map:Map<int64,int64>) (address:int64) -> map.Add(address, mem.Value))
          memoryMap
        |> fun newMap -> (mask, newMap)
      | InstrMask(newMask) -> (newMask, memoryMap))
    ({Value=""}, Map.empty)
  |> snd
  |> Map.toSeq
  |> Seq.map snd
  |> Seq.reduce (+)

printfn "Part 2: %i" (part2 instructions)