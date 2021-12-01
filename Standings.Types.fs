module Standings.Types

type TeamName = TeamName of string

type TeamScoreDto =
    { Name         : TeamName
      PointsScored : float }

type GameType =
    | Regular
    | Playoff

type GameDto =
    { Year : int
      Week : int
      Away : TeamScoreDto
      Home : TeamScoreDto
      GameType : GameType }

type Outcome = | Win | Loss | Tie

type Outcomes =
    { Wins   : int
      Losses : int
      Ties   : int }

type Points =
    { Scored : int
      Allowed : int }

type PointsAboveAverage =
    { Scored : float
      Allowed : float }

type TeamGame =
    { Opponent : TeamName
      Points   : Points
      Outcome  : Outcome }

type TeamStatLine =
    { Outcomes : Outcomes
      Points : Points
      PointsAboveAverage : PointsAboveAverage
      MarginOfVictory : float }

type SimpleRanking =
    { SimpleRankingScore : float
      StrengthOfSchedule : float }

type TeamRecord =
    { Team  : TeamName
      Games : TeamGame list }

type TeamStats =
    { Team  : TeamName
      Games : TeamGame list
      StatLine : TeamStatLine }

type TeamSummary =
    { Team  : TeamName
      Games : TeamGame list
      StatLine : TeamStatLine
      SimpleRanking : SimpleRanking
      WinRatio: float
      Pythagorean: float }

type LeaderboardRowDto =
    { Name: string
      Games: int
      Wins: int
      Losses: int
      Ties: int
      PointsFor: int
      PointsAgainst: int
      WinRatio: float
      PointsForPerGame: float
      PointsAgainstPerGame: float
      PointsForPerGameAboveAverage: float
      PointsAgainstPerGameBelowAverage: float
      PointDifferential: int
      MarginOfVictory: float
      StrengthOfSchedule: float
      SimpleRankingScore: float
      Pythagorean: float }

type BuildRecords =
    GameDto seq -> Map<TeamName, TeamRecord>

type CalculateStats =
    float -> Map<TeamName, TeamRecord> -> Map<TeamName, TeamStats>

type Summarize =
    Map<TeamName, TeamStats> -> Map<TeamName, TeamSummary>