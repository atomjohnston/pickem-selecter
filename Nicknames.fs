module Nicknames

let teamNames =
    Map[
        "ari", "cardinals";
        "atl", "falcons";
        "bal", "ravens";
        "buf", "bills";
        "car", "panthers";
        "chi", "bears";
        "cin", "bengals";
        "cle", "browns";
        "dal", "cowboys";
        "den", "broncos";
        "det", "lions";
        "gb",  "packers";
        "hou", "texans";
        "ind", "colts";
        "jac", "jaguars";
        "kc",  "chiefs";
        "lac", "chargers";
        "lar", "rams";
        "lv",  "raiders";
        "mia", "dolphins";
        "min", "vikings";
        "ne",  "patriots";
        "no",  "saints";
        "nyg", "giants";
        "nyj", "jets";
        "phi", "eagles";
        "pit", "steelers";
        "sea", "seahawks";
        "sf",  "49ers";
        "tb",  "buccaneers";
        "ten", "titans";
        "was", "team";
    ]
    |> Map.toSeq
    |> dict