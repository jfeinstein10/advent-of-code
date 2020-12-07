#! /usr/bin/env fsharpi

let lines = System.IO.File.ReadLines("04.input")

open System.Collections.Generic
open System.Text.RegularExpressions

let dictOfPairs pairsList =
        let d = new Dictionary<'a, 'b>()
        for (k,v) in pairsList do d.Add(k,v)
        d

let toPair (str:string) = (str.[0..2], str.[4..])
let printDict (dict:Dictionary<'a, 'b>) = printfn "start"; Seq.iter (fun key -> printfn "Key = %A" key) dict.Keys

let passports = 
        lines 
        |> Seq.fold (fun x y -> if y = "" then ""::x else match x with | [] -> [y] | head::tail -> (head+" "+y)::tail) []
        |> List.map (fun x -> x.Split ' ' |> Array.filter (fun x -> x.Length > 0) |> Array.map toPair)
        |> List.map dictOfPairs

let validFields = [
        "byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid"
]

let valid1 (passport:Dictionary<string,string>) = validFields |> List.map (passport.ContainsKey) |> List.reduce (&&)

let valid2 (passport:Dictionary<string,string>) =
        valid1 passport 
        && passport.Item("byr") >= "1920" && passport.Item("byr") <= "2002"
        && passport.Item("iyr") >= "2010" && passport.Item("iyr") <= "2020"
        && passport.Item("eyr") >= "2020" && passport.Item("eyr") <= "2030"
        && Regex.Match(passport.Item("hgt"), @"^((1[5-8][0-9]cm)|(19[0-3]cm)|(59in)|(6[0-9]in)|(7[0-6]in))$").Success
        && Regex.Match(passport.Item("hcl"), @"^#[0-9a-f]{6}$").Success
        && Regex.Match(passport.Item("ecl"), @"^((amb)|(blu)|(brn)|(gry)|(grn)|(hzl)|(oth))$").Success
        && Regex.Match(passport.Item("pid"), @"^[0-9]{9}$").Success

let soln1 (passports:Dictionary<string, string> list) valid =
        passports
                |> List.map valid
                |> List.map (fun x -> if x then 1 else 0)
                |> List.sum 

printfn "Part 1: %i" (soln1 passports valid1)
printfn "Part 2: %i" (soln1 passports valid2)
