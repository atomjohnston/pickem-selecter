module IO

open System

open FSharp.Data
open Standings.Types
open Picks.Types
open Cli.Functions

type GameRange =
    { Start : int
      End   : int }

let private asKey x y =
    x * 100 + y

module PickWeek =
    let asKey p =
        asKey p.Season p.Week

module GameDto =
    let asKey g =
        asKey g.Year g.Week

module GameRange =
    let parse s =
        let parse' =
            PickWeek.parse >> PickWeek.asKey

        if String.IsNullOrEmpty(s) then
            None
        else
            let parts = s.Split('-')
            let a, b =
                (parse' parts.[0],
                 parse' parts.[1])
            let range =
                if a < b
                then { Start = a; End = b }
                else { Start = b; End = a }
            Some range



let private row2game (r: CsvRow) =
    let parseType (s: string) =
          match (s.ToLower()) with
          | "p" -> Playoff
          | "r" -> Regular
          | _   -> failwith $"invalid game type {s}"

    { Year = r.["year"].AsInteger()
      Week = r.["week"].AsInteger()
      GameType = parseType r.["type"]
      Away =
          { Name = TeamName r.["away_team"]
            PointsScored = r.["away_points"].AsFloat() }
      Home =
          { Name = TeamName r.["home_team"]
            PointsScored = r.["home_points"].AsFloat() } }


let private allGames (csv: CsvFile) =
    let yr =
        csv.Take(1)
        |> (fun c -> c.Rows)
        |> Seq.exactlyOne
        |> (fun r -> r.["year"].AsInteger())

    csv.TakeWhile(fun r -> r.["year"].AsInteger() = yr)
        |> (fun c -> c.Rows)
        |> Seq.map row2game


let private someGames range (csv: CsvFile) =
    csv.Rows
    |> Seq.map (row2game >> (fun g -> {| Key = GameDto.asKey g; Game = g |}))
    |> Seq.sortBy (fun x -> x.Key)
    |> Seq.skipWhile (fun x -> x.Key < range.Start)
    |> Seq.takeWhile (fun x -> x.Key <= range.End)
    |> Seq.map (fun x -> x.Game)

let csv2games playoffs (range: GameRange option) (csvFile: string) =
    let csv = CsvFile.Load csvFile
    let result =
        match range with
        | None         -> allGames csv
        | Some range'  -> someGames range' csv
    if playoffs then
        result
    else
        result
        |> Seq.filter (fun g -> g.GameType = Regular)
