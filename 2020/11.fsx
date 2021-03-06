#! /usr/bin/env fsharpi

let map =
  System.IO.File.ReadLines("11.input")
  |> Seq.map (fun line -> line.ToCharArray())
  |> Seq.toArray
let seated = '#'
let empty = 'L'
let neighborDirections = 
  Seq.allPairs [-1..1] [-1..1] 
  |> Seq.filter (fun (x, y) -> x <> 0 || y <> 0)
let inBounds (r:int) (c:int) (map:char[][]) = 
  r >= 0 && r < map.Length && c >= 0 && c < map.[0].Length

let stepMap (changeFn:char[][] -> int -> int -> char) (map:char[][]): char[][] =
  map
  |> Array.mapi (fun rowIdx row -> 
    row
    |> Array.mapi (fun colIdx col -> changeFn map rowIdx colIdx))

let simulateUntilNoChanges (changeFn:char[][] -> int -> int -> char) (map:char[][]): char[][] =
  map
  |> Seq.unfold (fun lastMap -> 
    let nextMap = stepMap changeFn lastMap
    let equal = Array.compareWith (Array.compareWith (fun x y -> int x - int y)) lastMap nextMap
    if equal = 0 then None else Some(nextMap, nextMap))
  |> Seq.last

let countCells (map:char[][]) (value:char) =
  map 
  |> Array.map (fun row -> 
    row 
    |> Array.map (fun cell -> if cell = value then 1 else 0) 
    |> Array.sum)
  |> Array.sum

let part1 (map:char[][]): int =
  let neighborCount (map:char[][]) (row:int) (col:int): int =
    neighborDirections
    |> Seq.map (fun (rowOffset, colOffset) -> (row + rowOffset, col + colOffset))
    |> Seq.filter (fun (r, c) -> inBounds r c map && map.[r].[c] = seated)
    |> Seq.length
  let stepCell (map:char[][]) (row:int) (col:int): char =
    match map.[row].[col] with
    | '#' -> if neighborCount map row col >= 4 then empty else seated
    | 'L' -> if neighborCount map row col = 0 then seated else empty
    | _ -> map.[row].[col]
  countCells (simulateUntilNoChanges stepCell map) seated

printfn "Part 1: %i" (part1 map)

let part2 (map:char[][]): int =
  let neighborCount (map:char[][]) (row:int) (col:int): int =
    neighborDirections
    |> Seq.map (fun (rowOffset, colOffset) ->
      Seq.initInfinite (fun i -> (row + (i+1) * rowOffset, col + (i+1) * colOffset))
      |> Seq.takeWhile (fun (r, c) -> inBounds r c map)
      |> Seq.map (fun (r, c) -> map.[r].[c])
      |> Seq.filter ((<>) '.')
      |> Seq.map (fun v -> if v = seated then 1 else 0)
      |> Seq.tryHead)
    |> Seq.map (fun v -> match v with Some x -> x | None -> 0)
    |> Seq.sum
  let stepCell (map:char[][]) (row:int) (col:int): char =
    match map.[row].[col] with
    | '#' -> if neighborCount map row col >= 5 then empty else seated
    | 'L' -> if neighborCount map row col = 0 then seated else empty
    | _ -> map.[row].[col]
  countCells (simulateUntilNoChanges stepCell map) seated

printfn "Part 2: %i" (part2 map)