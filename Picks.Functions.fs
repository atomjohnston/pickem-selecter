module Picks.Functions

open Standings.Types
open Standings.Functions
open Picks.Types

module Pick =
    let rank i p =
        { p with
            Rank = Some i }

    let toArray p =
        [| p.Winner |> TeamName.name
           sprintf "%d" ((-1, p.Rank) ||> Option.defaultValue)
           sprintf "%.3f" p.Spread |]

module Matchup =
    let toArray (m: Matchup) =
        let (TeamName aw) = m.Away
        let (TeamName hm) = m.Home
        let append' b a =
            Array.append a b

        [|aw; hm|]
        |> append' (Pick.toArray m.Picks.[VegasAverage])
        |> append' (Pick.toArray m.Picks.[VegasMedian])
        |> append' (Pick.toArray m.Picks.[SimpleRankingScore])
        |> append' (Pick.toArray m.Picks.[SRSWithHomeField])
        |> append' (Pick.toArray m.Picks.[MarginOfVictory])
        |> append' (Pick.toArray m.Picks.[WinRatio])
        |> append' (Pick.toArray m.Picks.[Pythagorean])
        |> append' (Pick.toArray m.Picks.[AdjustedMarginOfVictory])


let private getCriteriaFunc hfa crit =
    let getSumry (a, h) =
        (a.TeamSummary, h.TeamSummary)

    let getSrs =
        getSumry >> (fun (a,h) -> (a.SimpleRanking, h.SimpleRanking))

    let calcVegas a h =
        ((h - a) / 2.0) //|> (*) -1.0

    match crit with
    | VegasAverage            -> (fun (a, h) -> calcVegas h.VegasOdds.Mean a.VegasOdds.Mean)
    | VegasMedian             -> (fun (a, h) -> calcVegas h.VegasOdds.Median a.VegasOdds.Median)
    | SimpleRankingScore      -> getSrs   >> (fun (a, h) -> (-) h.SimpleRankingScore a.SimpleRankingScore)
    | SRSWithHomeField        -> getSrs   >> (fun (a, h) -> (-) ((+) h.SimpleRankingScore hfa) a.SimpleRankingScore)
    | MarginOfVictory         -> getSumry >> (fun (a, h) -> (-) h.StatLine.MarginOfVictory a.StatLine.MarginOfVictory)
    | AdjustedMarginOfVictory -> getSumry >> (fun (a, h) -> (-) h.StatLine.PointsAboveAverage.Scored a.StatLine.PointsAboveAverage.Allowed)
    | WinRatio                -> getSumry >> (fun (a, h) -> (-) h.WinRatio a.WinRatio)
    | Pythagorean             -> getSumry >> (fun (a, h) -> (-) h.Pythagorean a.Pythagorean)


let private predictWinner awayTeam homeTeam delta =
    let winner =
        if delta >= 0.0
        then homeTeam
        else awayTeam

    { Winner = winner
      Spread = delta |> abs
      Rank = None }


let private pick fn (away: TeamSummaryWithOdds) (home: TeamSummaryWithOdds) : Pick =
    let delta = fn (away, home)
    predictWinner away.TeamSummary.Team home.TeamSummary.Team delta


let private rank crit matchups' =
    let folder (rank', rankedPicks) (m, p) =
        let rank = rank' + 1
        let rankedPick = p |> Pick.rank rank
        let rankedMatchup =
            { m with
                Picks = Map.add crit rankedPick m.Picks }
        rank, rankedMatchup::rankedPicks

    matchups'
    |> Seq.map (fun m -> m, Map.find crit m.Picks)
    |> Seq.sortBy (fun (_, a) -> a.Spread)
    |> Seq.fold folder (0, List.empty)
    |> snd


module Picker =
    let pickAll (hfa: float) (away: TeamSummaryWithOdds) (home: TeamSummaryWithOdds) : Matchup =
        let pickForCriteria crit =
            let pick' = pick (getCriteriaFunc hfa crit) away home
            Map.add crit pick'

        { Away = away.TeamSummary.Team
          Home = home.TeamSummary.Team
          Picks =
              Map.empty
              |> pickForCriteria VegasAverage
              |> pickForCriteria VegasMedian
              |> pickForCriteria SimpleRankingScore
              |> pickForCriteria SRSWithHomeField
              |> pickForCriteria MarginOfVictory
              |> pickForCriteria WinRatio
              |> pickForCriteria Pythagorean
              |> pickForCriteria AdjustedMarginOfVictory }

    let rankAll (matchups: Matchup seq) : Matchup seq =
        matchups
        |> rank VegasAverage
        |> rank VegasMedian
        |> rank SimpleRankingScore
        |> rank SRSWithHomeField
        |> rank MarginOfVictory
        |> rank WinRatio
        |> rank Pythagorean
        |> rank AdjustedMarginOfVictory
        |> Seq.ofList