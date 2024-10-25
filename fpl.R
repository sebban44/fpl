library(dplyr)
library(rvest)
library(jsonlite)
library(brms)

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
    was_home,
    round,
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

player_df$was_home <- ifelse(player_df$was_home == TRUE, 1, 0)
player_df$min_played <- scale(player_df$min_played)
player_df$bps <- scale(player_df$min_played)
player_df$influence <- scale(player_df$influence)
player_df$creativity <- scale(player_df$creativity)
player_df$threat <- scale(player_df$threat)

gkp_df <- player_df %>% filter(pos == "GKP")
player_df <- player_df %>% filter(!pos == "GKP")

f <- bf(pts ~  pos + min_played + was_home + xG + xA + xG_Inv + xGC + yellow_cards + red_cards + bps + (1 | name) + (1 | team) + (1 | opponent) )
fam <- gaussian()
pr <- c(
  set_prior("normal(0, 1)", class = "b"),        
  set_prior("normal(0, 1)", class = "sd", group = "name"),      
  set_prior("normal(0, 1)", class = "sd", group = "team"),       
  set_prior("normal(0, 1)", class = "sd", group = "opponent")
)

fit <- brm(
        formula = f,
        data = player_df,
        family = fam,
        prior = pr,
        chains = 4,
        cores = 4,
        backend = "cmdstanr"
)

summary(fit)

f_p <- bf(goals_scored ~  pos + min_played + xG + (1 | name) + (1 | team) + (1 | opponent) )
pr <- c(
  set_prior("normal(0, 1)", class = "b"),        
  set_prior("normal(0, 1)", class = "sd", group = "name"),      
  set_prior("normal(0, 1)", class = "sd", group = "team"),       
  set_prior("normal(0, 1)", class = "sd", group = "opponent")
)

fit_pois <- brm(
        formula = f_p,
        data = player_df,
        family = poisson("log"),
        prior = pr,
        chains = 4,
        cores = 4,
        backend = "cmdstanr"
)

summary(fit_pois)




pl <- data.frame(posterior_summary(fit,variable = "r_name"))

print(head(pl %>% arrange(desc(Estimate))))


pl_gr <- player_df %>% 
         group_by(name,pos) %>% 
         summarise(
            pts = mean(pts),
            starts = mean(starts),
            min_played = mean(min_played),
            xG = mean(xG),
            xA = mean(xA),
            xG_Inv = mean(xG_Inv),
            xGC = mean(xGC),
            yellow_cards = mean(yellow_cards),
            red_cards = mean(red_cards),
            bonus = mean(bonus),
            bps = mean(bps),
            influence = mean(influence),
            creativity = mean(creativity),
            threat = mean(threat)
           )


my_players <- data.frame(name=c("Haaland","Duran","Cunha","Maddison","Luis Díaz","M.Salah","Georginio","Aina","Digne","Alexander-Arnold"),
                         team=c("Man City","Aston Villa","Wolves","Spurs","Liverpool","Liverpool","Brighton","Nott'm Forest","Aston Villa","Liverpool"),
                         opponent=c("Wolves","Fulham","Man City","West Ham","Chelsea","Chelsea","Newcastle","Crystal Palace","Fulham","Chelsea")
                         )

my_players$pos <- factor(my_players$pos, levels = player_df$pos)



my_team <- inner_join(pl_gr,my_players,by=c("name"))

my_team$x_pts <- round(predict(player_model,my_team))
          
print(round(sum(my_team$x_pts)))

gkp_df <- gkp_df %>% filter(min_played > 0)

gk_model <- lmer(
   pts ~ min_played + saves + goals_conceded + clean_sheets + (1 | name) + (1 | team) + (1 | opponent),
   data = gkp_df
)

my_gk <- data.frame(name=c("Leno"), team = c("Fulham"), opponent = c("Aston Villa"))

gk_gr <- gkp_df %>% group_by(name) %>% summarise(min_played = mean(min_played), saves = mean(saves), goals_conceded = mean(goals_conceded), clean_sheets = mean(clean_sheets))

my_gk <- inner_join(gk_gr, my_gk,by=c("name"))

my_gk$x_pts <- round(predict(gk_model,my_gk))

predicted_points <- sum(my_team$x_pts,my_gk$x_pts) 

print(paste0("Predicted Points for Game Week: ", predicted_points))
