#! /usr/bin/env fsharpi

let lines = System.IO.File.ReadLines("06.input") |> Seq.toList

let part1 =
        lines
        |> Seq.fold
               (fun acc line ->
                    if line = "" 
                    then 
                        Set.empty::acc 
                    else 
                        match acc with 
                        | [] -> [Set.ofSeq line] 
                        | head::tail -> (Set.union head (Set.ofSeq line))::tail)
               []
        |> List.map Set.count
        |> List.sum

printfn "Part 1: %i" part1

let part2 =
        lines
        |> Seq.fold
               (fun acc line ->
                    if line = ""
                    then
                        (Set.ofSeq ['a'..'z'])::acc
                    else
                        match acc with
                        | [] -> [Set.ofSeq line]
                        | head::tail -> (Set.intersect head (Set.ofSeq line))::tail)
               []
        |> List.map Set.count
        |> List.sum

printfn "Part 2: %i" part2
