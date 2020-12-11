#! /usr/bin/env fsharpi

open System.Collections.Generic

let map =
  System.IO.File.ReadLines("11.input")
  |> Seq.map (fun line -> line.ToCharArray())
  |> Seq.toArray
let seated = '#'
let empty = 'L'
let rows = map.Length
let cols = map.[0].Length

let cross xs ys = xs |> List.collect (fun x -> ys |> List.map (fun y -> x, y))
let inBounds r c = r >= 0 && r < rows && c >= 0 && c < cols

let stepMap (changeFn:char[][] -> int -> int -> char) (map:char[][]): (char[][] * bool) =
  let mutable changes = false
  let newMap = 
    map
    |> Array.mapi (fun rowIdx row -> 
      row
      |> Array.mapi (fun colIdx col -> 
        let newCell = changeFn map rowIdx colIdx
        changes <- newCell <> map.[rowIdx].[colIdx] || changes
        newCell))
  (newMap, changes)

let simulateUntilNoChanges (changeFn:char[][] -> int -> int -> char) (map:char[][]): char[][] =
  let mutable map = map
  let mutable changes = true
  while changes do
    let (newMap, anyChanges) = stepMap changeFn map
    map <- newMap
    changes <- anyChanges
  map

let countCells (map:char[][]) (value:char) =
  map 
  |> Array.map (fun row -> 
    row 
    |> Array.map (fun cell -> if cell = value then 1 else 0) 
    |> Array.sum)
  |> Array.sum

let part1 (map:char[][]): int =
  let neighborCount (map:char[][]) (row:int) (col:int): int =
    cross [-1..1] [-1..1]
    |> List.filter (fun (x, y) -> x <> 0 || y <> 0)
    |> List.map (fun (rowOffset, colOffset) -> (row + rowOffset, col + colOffset))
    |> List.filter (fun (r, c) -> inBounds r c && map.[r].[c] = seated)
    |> List.length
  let stepCell (map:char[][]) (row:int) (col:int): char =
    match map.[row].[col] with
    | '#' -> if neighborCount map row col >= 4 then empty else seated
    | 'L' -> if neighborCount map row col = 0 then seated else empty
    | _ -> map.[row].[col]
  countCells (simulateUntilNoChanges stepCell map) seated

printfn "Part 1: %i" (part1 (Array.copy map))

let part2 (map:char[][]): int =
  let neighborCount (map:char[][]) (row:int) (col:int): int =
    cross [-1..1] [-1..1]
    |> List.filter (fun (x, y) -> x <> 0 || y <> 0)
    |> List.map (fun (rowOffset, colOffset) ->
      Seq.initInfinite (fun i -> (row + (i+1) * rowOffset, col + (i+1) * colOffset))
      |> Seq.takeWhile (fun (r, c) -> inBounds r c)
      |> Seq.map (fun (r, c) -> map.[r].[c])
      |> Seq.filter ((<>) '.')
      |> Seq.map (fun v -> if v = seated then 1 else 0)
      |> Seq.tryHead)
    |> List.map (fun v -> match v with Some x -> x | None -> 0)
    |> List.sum
  let stepCell (map:char[][]) (row:int) (col:int): char =
    match map.[row].[col] with
    | '#' -> if neighborCount map row col >= 5 then empty else seated
    | 'L' -> if neighborCount map row col = 0 then seated else empty
    | _ -> map.[row].[col]
  countCells (simulateUntilNoChanges stepCell map) seated

printfn "Part 2: %i" (part2 (Array.copy map))