library(dplyr)
library(rvest)
library(jsonlite)
library(brms)
library(lme4)

data <- fromJSON("https://fantasy.premierleague.com/api/bootstrap-static/")

teams <- data$teams
teams <- teams %>% select(team_id=id,team_name=name)
players <- data$elements
position <- data$element_type
position <- position %>% select(id,pos=singular_name_short)

players <- inner_join(players,position,by=c("element_type"="id"))

player_list <- players %>% select(id, team, name = web_name, pos ,selected_by_percent)

player_df <- data.frame()

#Go through all players to fetch match history
for (p in player_list$id) {
  player_url <- paste0("https://fantasy.premierleague.com/api/element-summary/", p, "/")
  player_data <- fromJSON(player_url)

  player_history <- player_data$history

  player_df <- bind_rows(player_df, player_history)
}

player_df <- inner_join(player_df,player_list,by=c("element" = "id")) 

#add team and pos to players
player_df <- inner_join(player_df, teams,by = c("team" = "team_id"))
player_df <- inner_join(player_df, teams,by = c("opponent_team" = "team_id"))

player_df <- player_df %>%
mutate(
   date = as.Date(kickoff_time)
  ) %>%
select(
    date,
    team = team_name.x,
    opponent = team_name.y,
    player_id=element,
    name,
    pos,
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

gkp_df <- player_df %>% filter(pos == "GKP")
player_df <- player_df %>% filter(!pos == "GKP")

#Convert to numeric variables
player_df$influence <- as.numeric(player_df$influence)
player_df$creativity <- as.numeric(player_df$creativity)
player_df$threat  <- as.numeric(player_df$threat)
player_df$ict_index <- as.numeric(player_df$ict_index)
player_df$xG <- as.numeric(player_df$xG)
player_df$xA <- as.numeric(player_df$xA)
player_df$xG_Inv <- as.numeric(player_df$xG_Inv)
player_df$xGC <- as.numeric(player_df$xGC)

#Factors
player_df$name <- as.factor(player_df$name)

#Scale variables

model <- lmer(
  pts ~ xG + xA + xG_Inv + xGC + bps + influence + threat + ict_index + creativity + min_played + (1 | name),
  data=player_df,
  #nAGQ=0,
  #control=glmerGrontrol(optimizer = "nloptwrap")
)

player_rank <- data.frame(ranef(model))

print(head(player_rank %>% arrange(desc(condval)), n =25))
