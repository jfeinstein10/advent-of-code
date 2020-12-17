#! /usr/bin/env fsharpi

let fields = 
  Map.empty
    .Add("departure location", [33..679] @ [691..971] |> Set.ofSeq)
    .Add("departure station", [48..646] @ [671..966] |> Set.ofSeq)
    .Add("departure platform", [37..601] @ [619..950] |> Set.ofSeq)
    .Add("departure track", [41..863] @ [875..973] |> Set.ofSeq)
    .Add("departure date", [37..145] @ [168..965] |> Set.ofSeq)
    .Add("departure time", [26..246] @ [257..972] |> Set.ofSeq)
    .Add("arrival location", [30..542] @ [556..960] |> Set.ofSeq)
    .Add("arrival station", [30..75] @ [89..954] |> Set.ofSeq)
    .Add("arrival platform", [48..274] @ [299..958] |> Set.ofSeq)
    .Add("arrival track", [41..561] @ [567..957] |> Set.ofSeq)
    .Add("class", [40..237] @ [243..952] |> Set.ofSeq)
    .Add("duration", [33..317] @ [336..972] |> Set.ofSeq)
    .Add("price", [47..365] @ [381..957] |> Set.ofSeq)
    .Add("route", [29..415] @ [435..951] |> Set.ofSeq)
    .Add("row", [45..762] @ [784..972] |> Set.ofSeq)
    .Add("seat", [34..888] @ [914..968] |> Set.ofSeq)
    .Add("train", [50..502] @ [513..960] |> Set.ofSeq)
    .Add("type", [45..802] @ [825..961] |> Set.ofSeq)
    .Add("wagon", [44..458] @ [475..963] |> Set.ofSeq)
    .Add("zone", [28..721] @ [735..972] |> Set.ofSeq)
let myTicket = [53;67;73;109;113;107;137;131;71;59;101;179;181;61;97;173;103;89;127;139]
let nearbyTickets = System.IO.File.ReadLines("16.input") |> Seq.map (fun ticket -> ticket.Split ',' |> Array.toSeq |> Seq.map int)

let allValidValues = 
  fields 
  |> Map.toSeq
  |> Seq.collect (fun (_, values) -> values)
  |> Set.ofSeq
let all (predicate:'a -> bool) (seq:seq<'a>): bool =
  seq |> Seq.map predicate |> Seq.reduce (&&)
let isTicketValid (ticket:seq<int>): bool =
  ticket
  |> all allValidValues.Contains

let part1 (nearbyTickets:seq<seq<int>>): int =
  let getErrorRate (ticket:seq<int>) =
    ticket
    |> Seq.filter (allValidValues.Contains >> not)
    |> Seq.sum
  Seq.sumBy getErrorRate nearbyTickets

printfn "Part 1: %i" (part1 nearbyTickets)

let part2 (fields:Map<string,Set<int>>) (nearbyTickets:seq<seq<int>>): int64 =
  let transpose (tickets:seq<seq<int>>) =
    let length = Seq.length (Seq.head tickets) - 1
    [0..length]
    |> Seq.map (fun idx -> tickets |> Seq.map (fun ticket -> ticket |> Seq.skip idx |> Seq.head))
  let join (a:Map<'a, 'b>) (b:Map<'a, 'b>): Map<'a, 'b> =
    Map(Seq.concat [(Map.toSeq a); (Map.toSeq b)]) 
  let remove (a:seq<'a>) (b:Set<'a>): seq<'a> =
    a |> Seq.filter (b.Contains >> not)
  let rec solveForFields (fieldOptions:seq<int*seq<string>>):Map<string,int> =
    let singleOptionFields =
      fieldOptions
      |> Seq.filter (fun (_, options) -> Seq.length options = 1)
      |> Seq.map (fun (idx, options) -> (Seq.head options, idx))
    if Seq.isEmpty singleOptionFields 
    then Map.empty 
    else
      let fieldsToRemove = singleOptionFields |> Seq.map fst |> Set.ofSeq
      let newFieldOptions = 
        fieldOptions
        |> Seq.map (fun (idx, options) -> (idx, remove options fieldsToRemove))
      join (Map.ofSeq singleOptionFields) (solveForFields newFieldOptions)

  let fieldOptions = 
    nearbyTickets
    |> transpose
    |> Seq.map (fun fieldValues -> 
      fields 
      |> Map.toSeq 
      |> Seq.filter (fun (_, validValues) -> fieldValues |> all validValues.Contains)
      |> Seq.map fst)
    |> Seq.mapi (fun idx x -> (idx, x))
  let solvedFields = solveForFields fieldOptions

  ["departure location"; "departure station"; "departure platform"; "departure track"; "departure date"; "departure time"]
  |> Seq.map (fun field -> int64 myTicket.[solvedFields.[field]])
  |> Seq.reduce (*)

printfn "Part 2: %i" (part2 fields (nearbyTickets |> Seq.filter isTicketValid))