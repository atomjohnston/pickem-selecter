module Cli.Types

open System.ComponentModel
open Spectre.Console.Cli


type GetScoresSettings() =
    inherit CommandSettings()

    [<Description("week to query (<year>:<week-number>, i.e. \"2021:1\" for the first week of the 2021 season)")>]
    [<CommandArgument(0, "<WEEK>")>]
    member val Week = ""
        with get, set

type RecordsFileSettings() =
    inherit CommandSettings()

    [<Description("game results file")>]
    [<CommandArgument(0, "<RECORDS_FILE>")>]
    member val RecordsFile = ""
        with get, set

type StandingsSettings() =
    inherit RecordsFileSettings()

    [<Description("output format (default: TABLE)")>]
    [<CommandOption("-o|--output")>]
    member val OutputType = "TABLE"
        with get, set

    [<Description("range of weeks, i.e. \"2020:2-2021:1\"")>]
    [<CommandOption("-r|--range")>]
    member val WeekRange = "" 
        with get, set

    [<Description("include playoffs")>]
    [<CommandOption("-p|--playoffs")>]
    member val IncludePlayoffs = false
        with get, set

type MakePicksSettings() =
    inherit StandingsSettings()

    [<Description("week to predict (<year>:<week-number>, i.e. \"2021:1\" for the first week of the 2021 season)")>]
    [<CommandArgument(1, "<PICK_WEEK>")>]
    member val PickWeek = ""
        with get, set
