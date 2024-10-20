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
player_df$team <- as.factor(player_df$team)
player_df$opponent <- as.factor(player_df$opponent)
player_df$pos <- as.factor(player_df$pos)

#Scale variables
#player_df$min_played <- scale(player_df$min_played, center=T, scale=T)
#player_df$bps <- scale(player_df$bps, center=T, scale=T)
#player_df$bonus <- scale(player_df$bonus, center=T, scale=T)
#player_df$influence <- scale(player_df$influence, center=T, scale=T)
#player_df$creativity <- scale(player_df$creativity, center=T, scale=T)
#player_df$value <- scale(player_df$value, center=T, scale=T)
#player_df$ict_index <- scale(player_df$ict_index, center=T, scale=T)
#player_df$threat <- scale(player_df$threat, center=T, scale=T)


player_model <- lmer(
  pts ~ pos + min_played + starts + xG + xA + xG_Inv + xGC + yellow_cards + red_cards  + (1 | name) + (1 | team) + (1 | opponent), 
  data = player_df
)

pl_gr <- player_df %>% 
         group_by(name,pos) %>% 
         summarise(
            min_played = mean(min_played),
            starts = mean(starts),
            xG = mean(xG),
            xA = mean(xA),
            xG_Inv = mean(xG_Inv),
            xGC = mean(xGC),
            yellow_cards = sum(yellow_cards),
            red_cards = sum(red_cards),
            goals_conceded = mean(goals_conceded),
            bps = mean(bps),
            bonus = mean(bonus),
            influence = mean(influence),
            creativity = mean(creativity)
           )

my_players <- data.frame(name=c("Haaland","Duran","Cunha","Maddison","Luis Díaz","M.Salah","Georginio","Aina","Digne","Alexander-Arnold"),
                         team=c("Man City","Aston Villa","Wolves","Spurs","Liverpool","Liverpool","Brighton","Nott'm Forest","Aston Villa","Liverpool"),
                         opponent=c("Wolves","Fulham","Man City","West Ham","Chelsea","Chelsea","Newcastle","Crystal Palace","Fulham","Chelsea")
                         )
my_team <- inner_join(pl_gr,my_players,by=c("name"))

my_team$x_pts <- round(predict(player_model,my_team))
          
print(round(sum(my_team$x_pts)))

gk_model <- lmer(
   pts ~ (1 | name) + (1 | team) + (1 | opponent),
   data = gkp_df
)

pred_gk <- data.frame(
  name = "Leno",
  team = "Fulham",
  opponent = "Aston Villa"
)

x_pts <- round(predict(player_model, pred_pl))
x_pts_gk <- round(predict(gk_model, pred_gk))

predicted_points <- sum(x_pts,x_pts_gk) + 7
