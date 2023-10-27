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

#Summarise  for grouped player data
player_summary <- player_df %>% 
    group_by(id,first_name,last_name) %>%
    summarise(
        mins = sum(min_played),
        points = sum(pts),
        xG = mean(xG,na.rm=TRUE),
        xA = mean(xA,na.rm=TRUE),
        xG_Inv = mean(xG_Inv,na.rm=TRUE),
        bonus = sum(bonus),
        bps = sum(bps),
        influence = mean(influence,na.rm=TRUE),
        creativity = mean(creativity,na.rm=TRUE),
        threat = mean(threat,na.rm=TRUE),
        ict_index = mean(ict_index,na.rm=TRUE),
        goals = sum(goals_scored),
        assists = sum(assists),
        clean_sheets = sum(clean_sheets),
        goals_conceded = sum(goals_conceded),
        own_goals = sum(own_goals),
        penalties_saved = sum(penalties_saved),
        penalties_missed = sum(penalties_missed),
        yellow_cards = sum(yellow_cards),
        red_cards = sum(red_cards),
        saves = sum(saves),
        starts = sum(starts),
        xGC = mean(xGC,na.rm=TRUE),
        selection_pct = last(selection_pct),
        value = last(value),
        transfers_in = last(transfers_in),
        transfers_out = last(transfers_out)
    ) 

#Do linear regression for points on data
model <- lm(points ~ mins+xG+xA+xG_Inv+xGC+bonus+bps+influence+creativity+threat+ict_index+goals+assists+clean_sheets+goals_conceded+own_goals+penalties_missed+yellow_cards+red_cards+starts, data = player_summary)

p <- predict(model,player_summary) %>% as.data.frame()
colnames(p) <- c("xPts")

#Compute the difference in actual vs expected points
player_summary$xPts <- p$xPts
player_summary <- player_summary %>% mutate(pts_diff = points - xPts)

#Show the top 15 players with highest expected points
head(player_summary %>% select(first_name,last_name,points,xPts, pts_diff) %>% arrange((pts_diff)),n=15)
