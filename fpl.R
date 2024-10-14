library(dplyr)
library(rvest)
library(jsonlite)

data <- fromJSON("https://fantasy.premierleague.com/api/bootstrap-static/")

teams <- data$teams
players <- data$elements

player_list <- players %>% select(id, first_name, second_name, selected_by_percent,)

player_df <- data.frame()

#Go through all players to fetch match history
for (p in player_list$id) {
  player_url <- paste0("https://fantasy.premierleague.com/api/element-summary/", p, "/")
  player_data <- fromJSON(player_url)

  player_history <- player_data$history

  player_df <- bind_rows(player_df, player_history)
}

player_df <- inner_join(player_df,player_list,by=c("element" = "id")) 

player_df <- player_df %>%
select(
    date = kickoff_time,
    id=element,
    first_name = first_name,
    last_name = second_name,
    selection_pct = selected_by_percent,
    pts = total_points,
    min_played = minutes,
    goals_scored,
    assists,
    clean_sheets,
    goals_conceded,
    own_goals,
    penalties_saved,
    penalties_missed,
    yellow_cards,
    red_cards,
    saves,
    bonus,
    bps,
    influence,
    creativity,
    threat,
    ict_index,
    starts,
    xG = expected_goals,
    xA = expected_assists,
    xG_Inv = expected_goal_involvements,
    xGC = expected_goals_conceded,
    value,
    transfers_balance,
    selected,
    transfers_in,
    transfers_out
)

#Convert to numeric variables
player_df$influence <- as.numeric(player_df$influence)
player_df$creativity <- as.numeric(player_df$creativity)
player_df$threat  <- as.numeric(player_df$threat)
player_df$ict_index <- as.numeric(player_df$ict_index)
player_df$xG <- as.numeric(player_df$xG)
player_df$xA <- as.numeric(player_df$xA)
player_df$xG_Inv <- as.numeric(player_df$xG_Inv)
player_df$xGC <- as.numeric(player_df$xGC)
