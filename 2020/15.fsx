#! /usr/bin/env fsharpi

let simulateGame (startingNumbers:int list) (turns:int) =
  let startingMap =
    startingNumbers
    |> Seq.mapi (fun idx num -> (num, [idx+1]))
    |> Seq.fold
      (fun (map:Map<int,int list>) (key:int, value:int list) -> map.Add(key, value))
      Map.empty
  [startingNumbers.Length+1..turns]
  |> Seq.fold
    (fun (lastNumber:int, map:Map<int,int list>) (turn:int) ->
      match map.TryFind(lastNumber) with
      | None -> raise (System.Exception("Last number not found"))
      | Some turnList ->
        let currentNumber =
          match turnList with
          | lastTurn::lastLastTurn::_ -> lastTurn - lastLastTurn
          | _ -> 0
        let newMap =
          match map.TryFind(currentNumber) with
          | None -> map.Add(currentNumber, [turn])
          | Some x -> map.Add(currentNumber, turn::x)
        (currentNumber, newMap))
    (Seq.last startingNumbers, startingMap)
  |> fst

let startingNumbers = [20; 9; 11; 0; 1; 2]
printfn "Part 1: %i" (simulateGame startingNumbers 2020)
printfn "Part 2: %i" (simulateGame startingNumbers 30000000)