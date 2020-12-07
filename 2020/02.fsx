#! /usr/bin/env fsharpi

let input = System.IO.File.ReadLines("02.input") |> Seq.toList

type Input = {Password:string; Letter:char; Min:int; Max:int}

let stringToInput (str:string) =
    let [|min; max; letter; _; password|] = str.Split [|' '; ':'; '-'|]
    {Password=password; Min=(int min); Max=(int max); Letter=(char letter) }

let inputs = List.map stringToInput input
let valid (input:Input) =
    let count = input.Password |> Seq.filter ((=) input.Letter) |> Seq.length
    let chars = input.Password.ToCharArray()
    (chars.[input.Min - 1] = input.Letter || chars.[input.Max - 1] = input.Letter) && chars.[input.Min - 1] <> chars.[input.Max - 1]

printfn "%i" (inputs |> List.filter valid |> List.length)
