module Standings.Functions

open System
open System.Linq
open Standings.Types


module Map =
    let valmap f m =
        m |> Map.map (fun _ x -> f x)

    let values m =
        m
        |> Map.toSeq
        |> Seq.map snd

module TeamName =
    let name (TeamName n) = n

module Outcomes =
    let sum (a: Outcomes) =
        a.Wins + a.Losses + a.Ties

    let sumf a =
        a
        |> sum
        |> float

module Points =
    let add (x: Points) (y: Points) : Points =
        { Scored  = x.Scored + y.Scored
          Allowed = x.Allowed + y.Allowed }

    let pythagorean (p: Points) =
        let exp = 2.37
        let for' = float p.Scored
        let against = float p.Allowed
        (for' ** exp) / ((for' ** exp) + (against ** exp))

module TeamRecord =
    let create name =
        { Team = name
          Games = List.empty }

module TeamStatLine =
    let empty =
        { Outcomes = { Wins   = 0; Losses  = 0; Ties = 0; }
          Points   = { Scored = 0; Allowed = 0 }
          PointsAboveAverage = { Scored = 0.; Allowed = 0. }
          MarginOfVictory = 0.0 }

module SimpleRanking =
    let empty =
        { SimpleRankingScore = 0.0
          StrengthOfSchedule = 0.0 }

module TeamSummary =
    let fromStats (stats:TeamStats) =
        { Team = stats.Team
          Games = stats.Games
          StatLine = stats.StatLine
          SimpleRanking = SimpleRanking.empty
          WinRatio = 0.
          Pythagorean = 0. }

module LeaderboardRowDto =
    let fromSummary (somery: TeamSummary) =
        let gameCount =
            somery.Games
            |> List.length
            |> float
        let ratio x = (float x) / gameCount
        let (TeamName name) = somery.Team
        let tsl = somery.StatLine
        { Name          = name
          Games         = int gameCount
          Wins          = tsl.Outcomes.Wins
          Losses        = tsl.Outcomes.Losses
          Ties          = tsl.Outcomes.Ties
          WinRatio      = somery.WinRatio
          PointsFor     = tsl.Points.Scored
          PointsAgainst = tsl.Points.Allowed
          Pythagorean   = somery.Pythagorean
          MarginOfVictory    = tsl.MarginOfVictory
          PointDifferential  = tsl.Points.Scored - tsl.Points.Allowed
          StrengthOfSchedule = somery.SimpleRanking.StrengthOfSchedule
          SimpleRankingScore = somery.SimpleRanking.SimpleRankingScore
          PointsForPerGame                 = ratio tsl.Points.Scored
          PointsAgainstPerGame             = ratio tsl.Points.Allowed
          PointsForPerGameAboveAverage     = tsl.PointsAboveAverage.Scored
          PointsAgainstPerGameBelowAverage = tsl.PointsAboveAverage.Allowed }

    let toArray s =
        let sf2 = sprintf "%.2f"
        let sf3 = sprintf "%.3f"
        [| s.Name
           string s.Games
           string s.Wins
           string s.Losses
           string s.Ties
           sf3 s.WinRatio
           string s.PointsFor
           string s.PointsAgainst
           string s.PointDifferential
           sf2 s.PointsForPerGame
           sf2 s.PointsAgainstPerGame
           sf2 s.MarginOfVictory
           sf2 s.StrengthOfSchedule
           sf2 s.SimpleRankingScore
           sf3 s.Pythagorean
           sf2 s.PointsForPerGameAboveAverage
           sf2 s.PointsAgainstPerGameBelowAverage |]

module Standings =
    let buildTeamRecords records game =
        let addScoresToRecord (us: TeamScoreDto) (them: TeamScoreDto) (tr: TeamRecord) =
            let getOutcome a b =
                match a with
                | a when a.PointsScored > b.PointsScored -> Win
                | a when a.PointsScored < b.PointsScored -> Loss
                | _ -> Tie

            let newGame =
                { Outcome = getOutcome us them
                  Opponent = them.Name
                  Points =
                  { Scored  = int us.PointsScored
                    Allowed = int them.PointsScored } }

            { tr with Games = newGame::tr.Games }

        let getStats name =
            match Map.tryFind name records with
            | Some x -> x
            | None   -> TeamRecord.create name

        let hStats = getStats game.Home.Name
        let aStats = getStats game.Away.Name

        records
        |> Map.add game.Home.Name (addScoresToRecord game.Home game.Away hStats)
        |> Map.add game.Away.Name (addScoresToRecord game.Away game.Home aStats)


    let statsFromRecord (avgPoints: float) (tr: TeamRecord) =
        let tally (a: Outcomes) (b: Outcome) =
            match b with
            | Win  -> { a with Wins   = (+) 1 a.Wins }
            | Loss -> { a with Losses = (+) 1 a.Losses }
            | Tie  -> { a with Ties   = (+) 1 a.Ties }

        let calcGameForLine (sl: TeamStatLine) (g: TeamGame) =
            { sl with
                Points   = Points.add sl.Points g.Points
                Outcomes = tally sl.Outcomes g.Outcome }

        let calcMov (sl: TeamStatLine) =
            let gameCount = sl.Outcomes |> Outcomes.sumf
            let mov = (float (sl.Points.Scored - sl.Points.Allowed)) / gameCount
            { sl with
                MarginOfVictory = mov
                PointsAboveAverage =
                { Scored  = (float sl.Points.Scored / gameCount) - avgPoints
                  Allowed = avgPoints - (float sl.Points.Allowed/ gameCount) } }

        { Team     = tr.Team
          Games    = tr.Games
          StatLine =
            tr.Games
            |> List.fold calcGameForLine TeamStatLine.empty
            |> calcMov }

    [<Struct>]
    type private Advanced = { MOV: float; SOS: float; SRS: float }

    module private Advanced =
        let applySos sos a =
            { a with
                SOS = sos
                SRS = sos + a.MOV }

        let adjustForAvg avgSrs a =
            { a with
                SOS = a.SOS - avgSrs
                SRS = a.SRS - avgSrs }

    [<Literal>]
    let private EPSILON = 0.000001

    let private init (team: TeamStats) =
        { MOV = team.StatLine.MarginOfVictory
          SRS = team.StatLine.MarginOfVictory
          SOS = 0.0 }

    let private getSummary (teamStats: TeamStats) advanced =
        let calcWinRatio =
            teamStats.Games
            |> List.length
            |> float
            |> (/) (float teamStats.StatLine.Outcomes.Wins)

        { Team          = teamStats.Team
          Games         = teamStats.Games
          StatLine      = teamStats.StatLine
          SimpleRanking =
              { SimpleRankingScore = advanced.SRS
                StrengthOfSchedule = advanced.SOS }
          WinRatio      = calcWinRatio
          Pythagorean   = Points.pythagorean teamStats.StatLine.Points }

    let private diffSos a sos' delta =
        (sos', a.SOS)
        ||> (-)
        |> abs
        |> max delta

    let private sos games (lookup: Collections.Generic.IDictionary<TeamName, Advanced>) =
        games |> List.averageBy (fun g -> lookup.[g.Opponent].SRS)

    let private averageRank (xs: Advanced seq) =
        xs |> Seq.averageBy (fun x -> x.SRS)

    let iterSummarize (stats: Map<TeamName,TeamStats>) : Map<TeamName, TeamSummary> =
        let mutable advancedStats = stats.ToDictionary((fun k -> k.Key), (fun v -> init v.Value))
        let mutable delta = 10.0

        while delta > EPSILON do
            delta <- 0.0
            for pair in advancedStats do
                let ts = pair.Value
                let sos' = sos stats.[pair.Key].Games advancedStats
                delta <- diffSos ts sos' delta
                advancedStats.[pair.Key] <- Advanced.applySos sos' ts
            //printf "%f\n" delta

        let avgSrs = advancedStats.Values |> averageRank

        for pair in advancedStats do
            advancedStats.[pair.Key] <- Advanced.adjustForAvg avgSrs pair.Value

        advancedStats
        |> Seq.map ((|KeyValue|) >> (fun (a, b) -> a, getSummary stats.[a] b))
        |> Map.ofSeq

    let recSummarize (stats: Map<TeamName,TeamStats>) =
        let folder (acc: float * Map<TeamName,Advanced>) (name: TeamName) (a: Advanced) =
            let delta, advanced = acc
            let xform =
                advanced
                |> sos stats.[name].Games
                |> (fun x -> Advanced.applySos x a)

            ( diffSos xform a.SOS delta,
              Map.add name xform advanced )

        let adjust advanced =
            let applyAvg =
                advanced
                |> Map.values
                |> averageRank
                |> Advanced.adjustForAvg
            advanced |> Map.valmap applyAvg

        let rec summarize (prev: Map<TeamName, Advanced>) =
            let delta, result = Map.fold folder (0.0, prev) prev
            //printf "%f\n" delta
            if delta <= EPSILON
            then result
            else summarize result

        let getSummary' k v = getSummary stats.[k] v

        let gameCount =
            stats
            |> Map.values
            |> Seq.head
            |> (fun x -> x.Games)
            |> List.length

        let stats' = stats |> Map.valmap init

        if gameCount = 1
        then stats' |> Map.map getSummary'
        else
            stats'
            |> summarize
            |> adjust
            |> Map.map getSummary'