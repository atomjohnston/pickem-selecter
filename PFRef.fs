module PFRef

open System

open FSharp.Data
open Standings.Types
open Picks.Types

module String =
    let contains (sub: string) (s: string) : bool =
        s.Contains(sub)

    let split (c: char) (s: string) : string[] =
        s.Split(c)

    let toLower (s: string) : string =
        s.ToLower()

module HtmlNode =
    let text (n: HtmlNode) =
        n.InnerText()

module GameDto =
    let toDelimited (sep: string) (g: GameDto) =
        let (TeamName aname) = g.Away.Name
        let (TeamName hname) = g.Home.Name
        String.Join(sep, g.Year, g.Week, aname, g.Away.PointsScored, hname, g.Home.PointsScored)


let formatWeekUrl (pw: PickWeek) : string =
    $"https://www.pro-football-reference.com/years/{pw.Season}/week_{pw.Week}.htm"

let gamesFromHtml (pw: PickWeek) (html: HtmlDocument) : GameDto seq =
    (*
    <tr class="date"><td colspan="3">Sep 13, 2020</td></tr>
    <tr class="loser"><td><a href="/teams/phi/2020.htm">Philadelphia Eagles</a></td><td class="right">17</td><td class="right gamelink"><a href="/boxscores/202009130was.htm">F<span class="no_mobile">inal</span></a></td></tr>
    <tr class="winner"><td><a href="/teams/was/2020.htm">Washington Football Team</a></td><td class="right">27</td><td class="right"/></tr>
    *)
    let teamAndScore (tr: HtmlNode) : TeamScoreDto =
        let cols = tr.Descendants "td" |> Seq.toArray
        let name =
            cols.[0] // <td><a href="/teams/phi/2020.htm">Philadelphia Eagles</a></td>
            |> HtmlNode.text
            |> String.split ' '
            |> Array.last
            |> String.toLower

        let parseResult =
            cols.[1] // <td class="right">17</td>
            |> HtmlNode.text
            |> Int32.TryParse

        let score =
            match parseResult with
            | (true, n)  -> n
            | (false, _) -> 0

        // ignore third column, which is the box score link

        { Name = TeamName name
          PointsScored = float score }

    html.CssSelect("div.game_summary .teams tbody")
    |> Seq.map (fun n ->
        n.Descendants "tr"
        |> Seq.tail // skip date: <tr class="date"><td colspan="3">Sep 13, 2020</td></tr>
        |> (fun xs ->
            { Year = pw.Season
              Week = pw.Week
              Away = xs |> Seq.head |> teamAndScore
              Home = xs |> Seq.last |> teamAndScore
              GameType = Regular }))
