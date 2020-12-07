#! /usr/bin/env fsharpi

let input = System.IO.File.ReadLines("01.input") |> Seq.toList |> List.map int

let rec soln lst = 
    match lst with
    | first :: rest -> 
        let sumTo2020 x = first + x = 2020
        let results = rest |> List.filter sumTo2020
        if results.Length > 0 then first * results.Head else soln rest

printfn "Part 1: %i" (soln input)

let soln2 lst = 

    let rec comb lstA depth =
        match lstA, depth with
        | _, 0 -> [[]]
        | [], _ -> []
        | head :: tail, _ -> 
            List.map ((@) [head]) (comb tail (depth - 1)) @ comb tail depth
    
    List.reduce (*)
        (comb lst 3
        |> List.filter (fun el -> List.sum el = 2020)
        |> List.head)

printfn "Part 2: %i" (soln2 input)
