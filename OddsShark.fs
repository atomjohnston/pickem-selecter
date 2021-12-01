namespace OddsShark

open System

open FSharp.Data

open Nicknames
open Picks.Types
open Standings.Types

module List =
    let dumbMedianBy (f: 'a -> float) (xs: 'a list) =
        let xs' = xs |> List.map f |> List.sort
        let len = List.length xs'
        let pivot = len / 2
        if len % 2 = 0 then
            (xs'.[pivot] + xs'.[pivot-1]) / 2.0
        else
            xs'.[pivot]


module Map =
    let ofList f xs =
        xs
        |> List.map (fun x -> (f x, x))
        |> Map.ofList


module OddsShark =
    [<Literal>]
    let Uri = "https://www.oddsshark.com/nfl/odds"

    let getGameSpreads (html: HtmlDocument) (standings: Map<TeamName, TeamSummary>) =
        let parseShortName attr =
            attr
            |> JsonValue.Parse
            |> (fun x -> x.["short_name"].AsString().ToLower())

        let parseAwaySpreads (hn: HtmlNode) =
            hn.CssSelect "div.op-first-row div.op-spread"
            |> List.map (HtmlNode.attributeValue "data-op-info" >> JsonValue.Parse >> (fun x -> x.["fullgame"].AsString()))

        let parseSpread (s: string) : float =
            if String.IsNullOrEmpty(s) then
                0.0
            else
                match s.[0] with
                | '-' -> s.Substring(1) |> float |> (*) -1.0
                | '+' -> s.Substring(1) |> float
                | 'E' -> 0.0
                | _   -> failwith "unknown spread"

        let buildSummary ((teams: string list), (awaySpreads: string list)) =
            //printfn "%s v. %s" teams.[0] teams.[1]
            let away =
                { TeamSummary = standings.[TeamName teamNames.[teams.[0]]]
                  VegasOdds =
                      { Mean   = awaySpreads |> List.averageBy parseSpread
                        Median = awaySpreads |> List.dumbMedianBy parseSpread } }

            let home =
                let { Mean = mean; Median = median } = away.VegasOdds
                { TeamSummary = standings.[TeamName teamNames.[teams.[1]]]
                  VegasOdds = { Mean = mean * -1.0; Median = median * -1.0 } }

            //printfn "%s v. %s" (away.ToString()) (home.ToString())
            [ away; home ]

        let matchups =
            html.CssSelect "div.op-matchup-team"
            |> List.map ((HtmlNode.attributeValue "data-op-name") >> parseShortName)
            |> List.chunkBySize 2

        html.CssSelect "div.op-item-row-wrapper.not-futures"
        |> List.map parseAwaySpreads
        |> List.zip matchups
        |> List.filter (fun (_,s) -> List.isEmpty s |> not)
        |> List.collect buildSummary
        |> List.distinctBy (fun x -> x.TeamSummary.Team)
        |> Map.ofList (fun x -> x.TeamSummary.Team)