library(tidyverse)
library(worldfootballR)

pl_shooting <- fb_league_stats(
  country = "ENG",
  gender = "M",
  season_end_year = 2024,
  tier = "1st",
  non_dom_league_url = NA,
  stat_type = "shooting",
  team_or_player = "player"
)
dplyr::glimpse(pl_shooting)