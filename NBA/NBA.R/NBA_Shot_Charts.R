library(tidyverse)
library(hexbin)
library(jsonlite)
library(httr)
library(scales)


percent_formatter = function(x) {
  scales::percent(x, accuracy = 1)
}

players_url = "http://stats.nba.com/stats/commonallplayers?LeagueID=00&Season=2019-20&IsOnlyCurrentSeason=0"

request_headers = c(
  `Connection` = 'keep-alive',
  `Accept` = 'application/json, text/plain, */*',
  `x-nba-stats-token` = 'true',
  `X-NewRelic-ID` = 'VQECWF5UChAHUlNTBwgBVw==',
  `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/78.0.3904.87 Safari/537.36',
  `x-nba-stats-origin` = 'stats',
  `Sec-Fetch-Site` = 'same-origin',
  `Sec-Fetch-Mode` = 'cors',
  `Referer` = 'https://stats.nba.com/players/leaguedashplayerbiostats/',
  `Accept-Encoding` = 'gzip, deflate, br',
  `Accept-Language` = 'en-US,en;q=0.9'
)
request = GET(players_url, add_headers(request_headers))

players_data = fromJSON(content(request, as = "text"))
players = tbl_df(data.frame(players_data$resultSets$rowSet[[1]], stringsAsFactors = FALSE))
names(players) = tolower(players_data$resultSets$headers[[1]])

players = mutate(players,
                 person_id = as.numeric(person_id),
                 rosterstatus = as.logical(as.numeric(rosterstatus)),
                 from_year = as.numeric(from_year),
                 to_year = as.numeric(to_year),
                 team_id = as.numeric(team_id)
)

if (Sys.Date() <= as.Date("2017-10-20")) {
  players = mutate(players, to_year = pmin(to_year, 2016))
}

players$name = sapply(players$display_last_comma_first, function(s) {
  paste(rev(strsplit(s, ", ")[[1]]), collapse = " ")
})

first_year_of_data = 1996
last_year_of_data = max(players$to_year)
season_strings = paste(first_year_of_data:last_year_of_data,
                       substr(first_year_of_data:last_year_of_data + 1, 3, 4),
                       sep = "-")
names(season_strings) = first_year_of_data:last_year_of_data

available_players = filter(players, to_year >= first_year_of_data)

names_table = table(available_players$name)
dupe_names = names(names_table[which(names_table > 1)])

available_players$name[available_players$name %in% dupe_names] = paste(
  available_players$name[available_players$name %in% dupe_names],
  available_players$person_id[available_players$name %in% dupe_names]
)

available_players$lower_name = tolower(available_players$name)
available_players = arrange(available_players, lower_name)

find_player_by_name = function(n) {
  filter(available_players, lower_name == tolower(n))
}

find_player_id_by_name = function(n) {
  find_player_by_name(n)$person_id
}

