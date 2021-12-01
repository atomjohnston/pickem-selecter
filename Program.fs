module Program

open System

open FSharp.Data
open Spectre.Console.Cli

open Standings.Types
open Standings.Functions
open Picks.Types
open Picks.Functions
open PFRef
open OddsShark
open Cli.Types
open Cli.Functions
open IO


let buildRecords : BuildRecords =
    fun games -> games |> Seq.fold Standings.buildTeamRecords Map.empty

let calcStats : CalculateStats =
    fun avg records -> records |> Map.map (fun _ x -> Standings.statsFromRecord avg x)

let summarize : Summarize =
    //Standings.iterSummarize
    Standings.recSummarize

let summariesToLeaderboard summary =
    summary
    |> Map.toSeq
    |> Seq.map (snd >> LeaderboardRowDto.fromSummary)


let downloadGames pw =
    let url = pw |> formatWeekUrl

    url
    |> HtmlDocument.Load
    |> gamesFromHtml pw


let pts tsd =
    float tsd.PointsScored


type StandingsCommand() =
    inherit Command<StandingsSettings>()

    override __.Execute(_, settings) =
        let render = settings.OutputType |> LeaderboardRowDto.getRenderer
        let compare a b =
            match a.WinRatio.CompareTo(b.WinRatio) with
            | 0 -> a.SimpleRankingScore.CompareTo(b.SimpleRankingScore)
            | n -> n
        let compare' a b = compare a b |> (*) -1

        let range = GameRange.parse settings.WeekRange

        let games =
            settings.RecordsFile
            |> csv2games settings.IncludePlayoffs range

        let avgPoints = games |> Seq.averageBy (fun g -> pts g.Home + pts g.Away) |> fun n -> n / 2.
        printfn "average points is %f" avgPoints

        games
        |> buildRecords
        |> calcStats avgPoints
        |> summarize
        |> summariesToLeaderboard
        |> Seq.sortWith compare'
        |> render

        0

type GetScoresCommand() =
    inherit Command<GetScoresSettings>()

    override __.Execute(_, settings) =
        let render game =
            printf "%s\n" (GameDto.toDelimited "," game)

        settings.Week
        |> PickWeek.parse
        |> downloadGames
        |> Seq.iter render

        0


type MakePicksCommand() =
    inherit Command<MakePicksSettings>()

    override __.Execute(_, settings) =
        let render = settings.OutputType |> Matchup.getRenderer

        let range = GameRange.parse settings.WeekRange
        let allGames = csv2games settings.IncludePlayoffs range settings.RecordsFile

        let avgPoints = allGames |> Seq.averageBy (fun g -> pts g.Home + pts g.Away) |> fun n -> n / 2.
        printfn "average points is %f" avgPoints

        let standingsWithOdds =
            allGames
            |> buildRecords
            |> calcStats avgPoints
            |> summarize
            |> OddsShark.getGameSpreads (HtmlDocument.Load OddsShark.Uri)

        let homeField = allGames |> Seq.averageBy (fun g -> pts g.Home - pts g.Away)
        printfn "home field is %f" homeField

        let pickAll' (g: GameDto) =
            Picker.pickAll homeField (standingsWithOdds.[g.Away.Name]) (standingsWithOdds.[g.Home.Name])

        settings.PickWeek
        |> PickWeek.parse
        |> downloadGames
        |> Seq.toList
        |> List.map pickAll'
        |> Picker.rankAll
        |> Seq.sortByDescending (fun m -> m.Picks.[SimpleRankingScore].Spread)
        |> render

        0


[<EntryPoint>]
let main argv =
    let app = CommandApp()
    app.Configure(fun config ->
        config.AddCommand<StandingsCommand>("standings")  |> ignore
        config.AddCommand<GetScoresCommand>("get-scores") |> ignore
        config.AddCommand<MakePicksCommand>("make-picks") |> ignore
        config.PropagateExceptions() |> ignore)
    try
        app.Run(argv)
    with
    | e -> printfn "%s\n" (e.ToString()); 0