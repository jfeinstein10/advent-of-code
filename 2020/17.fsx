#! /usr/bin/env fsharpi

let activeCubes = 
  System.IO.File.ReadLines("17.input") 
  |> Seq.mapi 
    (fun y line -> 
      line.ToCharArray() 
      |> Seq.mapi (fun x char -> (x, char))
      |> Seq.filter (fun (_, char) -> char = '#')
      |> Seq.map (fun (x, _) -> (x, y, 0)))
  |> Seq.concat
  |> Set.ofSeq

let stepCube (activeCubes:Set<'a>) (getNeighbors:'a -> Set<'a>) (cube:'a): bool =
  let activeNeighbors =
    getNeighbors cube
    |> Set.intersect activeCubes
    |> Set.count
  match activeCubes.Contains(cube) with
  | true -> activeNeighbors - 1 >= 2 && activeNeighbors - 1 <= 3
  | false -> activeNeighbors = 3
let step (getNeighbors:'a -> Set<'a>) (activeCubes:Set<'a>) (turn:int) = 
  printfn "Step: %i" turn
  activeCubes 
  |> Seq.map getNeighbors
  |> Set.unionMany
  |> Set.filter (stepCube activeCubes getNeighbors)

let rec part1 (initialActiveCubes:Set<int*int*int>) (steps:int): int =
  let neighbors ((activeX, activeY, activeZ):int*int*int): Set<int*int*int> =
    Seq.allPairs [-1..1] [-1..1]
    |> Seq.allPairs [-1..1]
    |> Seq.map (fun (x, (y, z)) -> (x+activeX, y+activeY, z+activeZ))
    |> Set.ofSeq
  [0..steps-1]
  |> Seq.fold (step neighbors) initialActiveCubes
  |> Set.count

printfn "Part 1: %i" (part1 activeCubes 6)

let rec part2 (initialActiveCubes:Set<int*int*int*int>) (steps:int): int =
  let neighbors ((activeX, activeY, activeZ, activeW):int*int*int*int): Set<int*int*int*int> =
    Seq.allPairs [-1..1] [-1..1]
    |> Seq.allPairs [-1..1]
    |> Seq.allPairs [-1..1]
    |> Seq.map (fun (x, (y, (z, w))) -> (x+activeX, y+activeY, z+activeZ, w+activeW))
    |> Set.ofSeq
  [0..steps-1]
  |> Seq.fold (step neighbors) initialActiveCubes
  |> Set.count

let activeCubes4d =
  activeCubes
  |> Set.map (fun (x,y,z) -> (x,y,z,0))
printfn "Part 2: %i" (part2 activeCubes4d 6)