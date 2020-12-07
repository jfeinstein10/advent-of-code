#! /usr/bin/env fsharpi

open System

let graph =
    System.IO.File.ReadLines("07.input")
    |> Seq.toList
    |> List.map (fun line -> line.Replace(" bags", ""))
    |> List.map (fun line -> line.Replace(" bag", ""))
    |> List.map (fun line -> line.Replace(".", ""))
    |> List.map (fun line -> line.Split([|" contain "|], StringSplitOptions.RemoveEmptyEntries))
    |> List.map (fun line -> (line.[0], line.[1]))

let soln1 (graph:(string*string) list) (target:string) =
    let rec set (target:string) =
        let containerSet =
            graph
            |> List.filter (fun (_, contains) -> contains.Contains(target))
            |> List.map (fun (container, _) -> container)
            |> Set.ofList
        let recSets = Set.unionMany (containerSet |> Set.map (fun container -> set container))
        Set.union containerSet recSets
    set target |> Set.count

printfn "Part 1: %i" (soln1 graph "shiny gold")

let soln2 (graph:(string*string) list) (target:string):int =
    let rec childCount (target:string):int =
        graph
        |> List.filter (fun (container, _) -> container = target)
        |> List.map (fun (_, children) -> children.Split([|", "|], StringSplitOptions.RemoveEmptyEntries) |> Seq.toList)
        |> List.concat
        |> List.filter (fun child -> child <> "no other")
        |> List.map (fun child -> (int child.[0] - int '0') * (1 + childCount child.[2..]))
        |> List.sum
    childCount target

printfn "Part 2: %i" (soln2 graph "shiny gold")
