library(dplyr)
library(rvest)
library(jsonlite)


data <- fromJSON("https://fantasy.premierleague.com/api/bootstrap-static/")

teams <- data$teams
players <- data$elements
position <- data$element_type

position <- position %>% select(id,pos=singular_name_short)
teams <- teams %>% select(id,team_name=short_name)

players <- inner_join(players,position,by=c("element_type"="id"))
players <- inner_join(players, teams, by=c("team"="id"))
gk_list <- players %>% filter(pos == "GKP")
player_list <- players %>% filter(!pos == "GKP")

#At least 90 mins playtime
players <- players %>% filter(minutes >= 90)

player_list <- players %>% select(id, team_name, pos, name=web_name, starts, minutes, form,
                                  xG=expected_goals_per_90, xG_inv=expected_goal_involvements_per_90,
                                  xA=expected_assists_per_90,xGA=expected_goals_conceded_per_90,
                                  bonus,bps,influence,creativity,threat,ict_index,
                                  ppg=points_per_game,total_pts=total_points,
                                  status, sel=selected_by_percent,cost=now_cost)

gk_list <- gk_list %>% select(id, team_name, pos, name=web_name, starts, minutes, form,
                                  saves=saves_per_90,xGA=expected_goals_conceded_per_90,
                                  bonus,bps,influence,creativity,threat,ict_index,
                                  ppg=points_per_game,total_pts=total_points,
                                  status, sel=selected_by_percent,cost=now_cost)

player_list <- player_list %>% mutate(pts = (total_pts / minutes) * 90)
gk_list <- gk_list %>% mutate(pts = (total_pts / minutes) * 90))

library(lme4)

# Fit the model using glmer (frequentist approach)
model_players <- glmer(
  pts ~ xG + xG_inv + xA + xGA + influence + creativity + threat + (1 | id),
  data = player_list,
  family = binomial(link = "logit")
)

# Summary of the model
summary(model_players)
