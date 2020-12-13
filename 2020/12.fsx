#! /usr/bin/env fsharpi

open System
open System.Text.RegularExpressions

type Step = {Direction:char; Amount:int}
type Point =
  {X:int; Y:int}
  static member (+) (p1:Point, p2:Point) = {X = p1.X + p2.X; Y = p1.Y + p2.Y}
  static member (*) (m, p:Point) = {X = m * p.X; Y = m * p.Y}

let parseStep (step:string): Step =
  let m = Regex.Match(step, "^([NSEWFLR])([0-9]+)$")
  {Direction = char m.Groups.[1].Value; Amount = int m.Groups.[2].Value}
let navSteps =
  System.IO.File.ReadLines("12.input")
  |> Seq.map parseStep

let direction (step:Step): Point =
  match step.Direction with
  | 'N' -> {X =  0; Y =  1}
  | 'S' -> {X =  0; Y = -1}
  | 'E' -> {X =  1; Y =  0}
  | 'W' | _ -> {X = -1; Y =  0}

let part1 (navSteps:seq<Step>): int =
  let directions = [|'N'; 'E'; 'S'; 'W'|]
  let rec turn (facing:char) (step:Step) =
    let turns = (step.Amount) / 90
    let direction = if step.Direction = 'R' then 1 else -1
    let nextIndex = (turns * direction + (Array.findIndex ((=) facing) directions)) % directions.Length
    let wrapped = if sign nextIndex < 0 then nextIndex + directions.Length else nextIndex
    directions.[wrapped]
  let rec takeStep ((facing, point):char*Point) (step:Step): char*Point =
    match step.Direction with
    | 'N' | 'S' | 'E' | 'W' -> facing, point + step.Amount * direction step
    | 'F' -> takeStep (facing, point) {Direction = facing; Amount = step.Amount}
    | 'L' | 'R' | _ -> (turn facing step, point)
  navSteps
  |> Seq.fold takeStep ('E', {X = 0; Y = 0})
  |> fun (_, point) -> abs point.X + abs point.Y

printfn "Part 1: %i" (part1 navSteps)

let part2 (navSteps:seq<Step>): int =
  let rec rotateWaypoint (waypoint:Point) (step:Step): Point =
    match step.Amount with
    | 0 -> waypoint
    | _ ->
      match step.Direction with
      | 'L' -> rotateWaypoint {X = -waypoint.Y; Y = waypoint.X} {Direction = step.Direction; Amount = step.Amount-90}
      | 'R' | _ -> rotateWaypoint {X = waypoint.Y; Y = -waypoint.X} {Direction = step.Direction; Amount = step.Amount-90}
  let rec takeStep ((waypoint, point):Point*Point) (step:Step): Point*Point =
    match step.Direction with
    | 'N' | 'S' | 'E' | 'W' -> waypoint + step.Amount * direction step, point
    | 'F' -> waypoint, point + step.Amount * waypoint
    | 'L' | 'R' | _ -> (rotateWaypoint waypoint step, point)
  navSteps
  |> Seq.fold takeStep ({X = 10; Y = 1}, {X = 0; Y = 0})
  |> fun (_, lastPosition) -> abs lastPosition.X + abs lastPosition.Y

printfn "Part 2: %i" (part2 navSteps)