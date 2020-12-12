#! /usr/bin/env fsharpi

open System.Text.RegularExpressions

let parseStep (step:string):char*int =
  let m = Regex.Match(step, "^([NSEWFLR])([0-9]+)$")
  (char m.Groups.[1].Value, int m.Groups.[2].Value)
let navSteps = 
  System.IO.File.ReadLines("12.input")
  |> Seq.map parseStep

let part1 (navSteps:seq<char*int>):int = 
  let directions = [|'N'; 'E'; 'S'; 'W'|]
  let rec turn (facing:char) (step:char*int) =
    let turns = (snd step) / 90
    let direction = if fst step = 'R' then 1 else -1
    let nextIndex = (turns * direction + (Array.findIndex ((=) facing) directions)) % directions.Length
    let wrapped = if sign nextIndex < 0 then nextIndex + directions.Length else nextIndex
    directions.[wrapped]
  let rec takeStep ((facing, x, y):char*int*int) (step:char*int):char*int*int =
    match fst step with
    | 'N' -> (facing, x, y + snd step)
    | 'S' -> (facing, x, y - snd step)
    | 'E' -> (facing, x + snd step, y)
    | 'W' -> (facing, x - snd step, y)
    | 'F' -> takeStep (facing, x, y) (facing, snd step)
    | 'L' | 'R' | _ -> (turn facing step, x, y)
  navSteps
  |> Seq.fold takeStep ('E', 0, 0)
  |> fun (_, lastX, lastY) -> abs lastX + abs lastY

printfn "Part 1: %i" (part1 navSteps)

let part2 (navSteps:seq<char*int>):int =
  let rec rotateWaypoint waypointX waypointY (direction, degrees) =
    match degrees with
    | 0 -> waypointX, waypointY
    | _ -> 
      match direction with
      | 'L' -> rotateWaypoint -waypointY waypointX (direction, degrees-90)
      | 'R' | _ -> rotateWaypoint waypointY -waypointX (direction, degrees-90)
  let rec takeStep ((waypointX, waypointY, x, y):int*int*int*int) (step:char*int):int*int*int*int =
    match fst step with
    | 'N' -> (waypointX, waypointY + snd step, x, y)
    | 'S' -> (waypointX, waypointY - snd step, x, y)
    | 'E' -> (waypointX + snd step, waypointY, x, y)
    | 'W' -> (waypointX - snd step, waypointY, x, y)
    | 'F' -> (waypointX, waypointY, x + snd step * waypointX, y + snd step * waypointY)
    | 'L' | 'R' | _ -> 
      let (newWaypointX, newWaypointY) = rotateWaypoint waypointX waypointY step
      (newWaypointX, newWaypointY, x, y)
  navSteps
  |> Seq.fold takeStep (10, 1, 0, 0)
  |> fun (_, _, lastX, lastY) -> abs lastX + abs lastY

printfn "Part 2: %i" (part2 navSteps)