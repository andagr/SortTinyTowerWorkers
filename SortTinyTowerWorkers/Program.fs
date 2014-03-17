open System
open System.IO
open FSharp.Data

type String with
    member this.ciCompare other =
        System.String.Equals(this, other, StringComparison.InvariantCultureIgnoreCase)

type Color =
    | Green
    | Blue
    | Yellow
    | Purple
    | Orange
    with
        static member Parse color =
            match color with
            | "green" -> Color.Green
            | "blue" -> Color.Blue
            | "yellow" -> Color.Yellow
            | "purple" -> Color.Purple
            | "orange" -> Color.Orange
            | _ -> raise(ArgumentException("Invalid color"))
        override this.ToString() =
            match this with
            | Green -> "Green"
            | Blue -> "Blue"
            | Yellow -> "Yellow"
            | Purple -> "Purple"
            | Orange -> "Orange"

type Skill(color: Color, value: int) =
    do if value < 0 || value > 9 then raise (ArgumentException("Invalid skill value: " + sprintf "%i" value))
    member this.Color = color
    member this.Value = value

type Bitizen(name: string, dreamJob: string, skills: Skill[]) =
    do if skills.Length <> 5 then raise(ArgumentException("Invalid number of skills"))
    let skillSum = skills |> Array.sumBy (fun s -> s.Value) |> decimal
    member this.Name = name
    member this.DreamJob = dreamJob
    member this.SortValueFor (color: Color) =
 (*
          |
          V
        0,9,0 => 9*10 + 10-0 = 100
        1,9,1 => 9*10 + 10-1 = 99
        8,9,1 => 9*10 + 10-4,5 = 95,5
        8,9,8 => 9*10 + 10-8 = 92
        9,9,9 => 9*10 + 10-9 = 91
        0,8,0 => 8*10 + 10-0 = 90

        skill * 10 + 10 - avg(rest of skills)

        I.e., sort bitizens so that at an equal skill level then specialists are ranked higher than generalists.
*)
        let matchingSkill = decimal (skills |> (Array.find (fun s -> s.Color = color))).Value
        let avgOtherskills = (skillSum - matchingSkill) / 4m
        0m - (matchingSkill * 10m + 10m - avgOtherskills)
    override this.ToString() =
        name + ", " + dreamJob + ", [| " + String.Join("; ", skills |> Array.map (fun s -> string s.Color + ": " + string s.Value)) + " |]"

type Job = 
    { Name: string; Color: Color; }
    with
        override this.ToString() =
            this.Name + ", " + string this.Color

type Position = 
    { Job: Job; Employee: Bitizen; }
    with
        override this.ToString() =
            let jobStr = string this.Job
            jobStr + String.replicate (25 - jobStr.Length) " " + string this.Employee

let fillPositions filter (jobs: Job list) (bitizens: Bitizen list) =
    let fillOne (job: Job) (bitizens: Bitizen list) filter =
        let matches, rest = List.partition (fun b -> filter job b) bitizens
        let sortedMatches = matches |> List.sortBy (fun b -> b.SortValueFor job.Color)
        match sortedMatches with
        | [] -> None, bitizens
        | head::tail  -> Some { Job = job; Employee = head }, tail@rest
    let rec fillPositionsInner (jobs: Job list) (bitizens: Bitizen list) (positions: Position list) =
        match jobs with
        | [] -> positions, bitizens
        | head::tail -> match fillOne head bitizens filter with
                        | None, b -> fillPositionsInner tail b (positions)
                        | Some p, b -> fillPositionsInner tail b (p::positions)
    fillPositionsInner jobs bitizens []

[<EntryPoint>]
let main argv =
    let jobs = 
        (new CsvProvider<"shops.csv">()).Data
        |> Seq.map (fun r -> seq { for i in 1..3 -> { Name = r.Name; Color = Color.Parse r.Color }})
        |> Seq.collect (fun o -> seq { yield! o })
        |> List.ofSeq

    let bitizens = 
        (new CsvProvider<"bitizens.csv">()).Data
        |> Seq.map
            (fun r ->
                Bitizen(r.Name, r.DreamJob, 
                    [| 
                        Skill(Color.Blue, r.Blue);
                        Skill(Color.Green, r.Green);
                        Skill(Color.Orange, r.Orange);
                        Skill(Color.Purple, r.Purple);
                        Skill(Color.Yellow, r.Yellow)
                    |]))
        |> List.ofSeq

    let dreamPositions, bitizens = fillPositions (fun j b -> j.Name.ciCompare(b.DreamJob)) jobs bitizens
    let normalPositions, bitizens = fillPositions (fun j b -> true) jobs bitizens

    File.WriteAllLines("positions.txt", dreamPositions@normalPositions |> List.map (fun p -> string p))

    0