library(dplyr)
library(rvest)
library(jsonlite)
library(brms)

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
gk_list <- gk_list %>% mutate(pts = (total_pts / minutes) * 90)

player_list$pts <- ifelse(is.na(player_list$pts, 0, player_list$pts))

model_players <- brm(
  pts ~ xG + xG_inv + xA + xGA + influence + creativity + threat  + (1 | id),
  data = player_list,
  family = gaussian(),
  prior = c(
    prior(normal(0, 1), class = "b"),
    prior(normal(0, 1), class = "sd")
  ),
  chains = 4,
  cores = 4
)

# For goalkeepers
#model_goalkeepers <- brm(
  #pts ~ saves + xGA + influence + creativity + threat + ict_index + (1 | team_name) + (1 | id),
  #data = gk_list,
  #family = gaussian(),
  #prior = c(
    #prior(normal(0, 1), class = "b"),
    #prior(cauchy(0, 2), class = "sd")
  #),
  #chains = 4,
  #cores = 4,
  #iter = 4000
#)


