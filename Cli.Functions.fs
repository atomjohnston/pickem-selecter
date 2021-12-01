module Cli.Functions

open System
open Spectre.Console

open Standings.Functions
open Picks.Types
open Picks.Functions


module PickWeek =
    let parse (s: string) : PickWeek =
        let parts = s.Split(':', 2)
        { Season = parts.[0] |> int
          Week   = parts.[1] |> int }

let (|CSV|TSV|TABLE|) (s: string) =
    match s.ToUpper() with
    | "CSV"   -> CSV
    | "TSV"   -> TSV
    | "TABLE" -> TABLE
    | x  -> failwith $"no output type {x}"


let renderTable (headers: string[]) (f: 'T -> string[]) (board: 'T seq) : unit =
    let table = Table()
    table.AddColumns(headers) |> ignore
    for row in board do
        table.AddRow(f row) |> ignore
    AnsiConsole.Render(table)

let toDelimited (sep: string) (s: string[]) =
    String.Join(sep, s)

let renderDelimited char csvHeaders f board =
    Console.WriteLine(String.concat char csvHeaders)
    board
    |> Seq.map (f >> (toDelimited char))
    |> Seq.iter (fun s -> Console.WriteLine(s))


let getRenderer format headers f =
    match format with
    | CSV   -> renderDelimited ","  headers f
    | TSV   -> renderDelimited "\t" headers f
    | TABLE -> renderTable headers f

module LeaderboardRowDto =
    let csvHeaders =
        [| "team"; "g"; "w"; "l"; "t"; "w%"; "pf"; "pa"; "pd"; "pf/g"; "pa/g"; "mov"; "sos"; "srs"; "pyth"; "pfaa/g"; "paaa/g" |]

    let getRenderer format =
        getRenderer format csvHeaders LeaderboardRowDto.toArray

module Matchup =
    let headers =
        "away,home,vgs,vgs_r,vgs_d,vgs',vgs_r',vgs_d',srs,srs_r,srs_d,srs',srs'_r,srs'_d,mov,mov_r,mov_d,w%,w%_r,w%_d,pyth,pyth_r,pyth_d,amov,amov_r,amov_d".Split(',')

    let toCsv m =
        String.Join(',', (Matchup.toArray m))

    let getRenderer format =
        getRenderer format headers Matchup.toArray