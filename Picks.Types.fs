module Picks.Types

open Standings.Types

type PickWeek =
    { Season: int
      Week: int }

type Criteria =
    | VegasAverage
    | VegasMedian
    | SimpleRankingScore
    | SRSWithHomeField
    | MarginOfVictory
    | WinRatio
    | Pythagorean
    | AdjustedMarginOfVictory

type Pick =
    { Winner: TeamName
      Spread: float
      Rank: int option }

type Matchup =
    { Away: TeamName
      Home: TeamName
      Picks: Map<Criteria, Pick> }

type VegasOdds =
    { Mean: float
      Median : float }

type TeamSummaryWithOdds =
    { TeamSummary : TeamSummary
      VegasOdds : VegasOdds }