#----------------------------------------------------------------------------------------------#
#                                                                                              #
#                       create a football betting app - data work                              #
#                                                                                              #
#----------------------------------------------------------------------------------------------#

##  libraries  ##
library(openxlsx)
library(XLConnect)
library(tidyverse)

#------------------------------------------------------------------------#
#                             data import                                #
#------------------------------------------------------------------------#

tmp <- tempfile(fileext = ".xlsx")
download.file(url = "http://football-data.co.uk/mmz4281/1718/all-euro-data-2017-2018.xlsx", destfile = tmp, mode = "wb")

sheets <- getSheetNames(tmp)
SheetList <- lapply(sheets, read.xlsx, xlsxFile = tmp)
names(SheetList) <- sheets
data <- do.call("bind_rows", SheetList)

#------------------------------------------------------------------------#
#                        data transformation                             #
#------------------------------------------------------------------------#

##  add columns for total cards, goals, corners, cards, shots, btts

data <- data %>%
  mutate(total_HTgoals = rowSums(data[, c("HTHG", "HTAG")])) %>% 
  mutate(total_FTgoals = rowSums(data[, c("FTHG", "FTAG")])) %>% 
  mutate(total_shots = rowSums(data[, c("HS", "AS")])) %>% 
  mutate(total_shots_ontarget = rowSums(data[, c("HST", "AST")])) %>% 
  mutate(total_corners = rowSums(data[, c("HC", "AC")])) %>%
  mutate(total_fouls = rowSums(data[, c("HF", "AF")])) %>% 
  mutate(total_yellow = rowSums(data[, c("HY", "AY")])) %>%
  mutate(total_red = rowSums(data[, c("HR", "AR")])) %>% 
  mutate(total_cards = rowSums(data[, c("HR", "AR", "HY", "AY")])) %>% 
  mutate(btts = ifelse(FTHG > 0 & FTAG > 0, 1, 0))
  
##  split data into home and away teams  ##
home_teams <- data[, c("Div", "HomeTeam", "FTHG", "HTHG", "Referee", "HS", "HST", "HC", "HF", "HY", "HR", 
                       "total_HTgoals", "total_FTgoals", "total_shots", "total_shots_ontarget", "total_corners",
                       "total_fouls", "total_yellow", "total_red", "total_cards", "btts")]
away_teams <- data[, c("Div", "AwayTeam", "FTAG", "HTAG", "Referee", "AS", "AST", "AC", "AF", "AY", "AR", 
                       "total_HTgoals", "total_FTgoals", "total_shots", "total_shots_ontarget", "total_corners",
                       "total_fouls", "total_yellow", "total_red", "total_cards", "btts")] 

##  combine data  ##
names(away_teams) <- names(home_teams)
data_full <- bind_rows(home_teams, away_teams)

##  change colnames  ##
#colnames(data_full) <- c("League", "Team", "Team goals", "HT Team goals", "Referee", "Team Shots", "Team Shots on target",
                         #"Team corners", "Team fouls", "Team yellow cards", "Team red cards", "HT goals", "Total goals", 
                         #"Total shots", "Total shots on target", "Total corners", "Total fouls", "Total yellow cards",
                         #"Total red cards", "BTTS")

##  change league names  ##
data_full$Div <- as.factor(data_full$Div)
levels(data_full$Div) <- c("Belgium - Jupiler League", "Germany - Bundesliga", "Germany - 2. Bundesliga", 
                              "England - Premier League", "England - Championship", "England - League One", 
                              "England - League Two", "England - National League", "France - Ligue 1", "France - Ligue 2",
                              "Greece - Super League", "Italy - Serie A", "Italy - Serie B", "Netherlands - Eredivisie",
                              "Portugal - Primeira Liga", "Scotlang - Premiership", "Scotland - Championship", 
                              "Scotland - League One", "Scotland - League Two", "Spain - La Liga", "Spain - La Liga 2", 
                              "Turkey - Super Lig")

#------------------------------------------------------------------------#
#                        calculations for bets                           #
#------------------------------------------------------------------------#

data_goals <- data_full %>% 
  group_by(Div, HomeTeam) %>% 
  summarize(share_btts = sum(btts) / length(btts),
            goals_per_game = sum(total_FTgoals) / length(btts),
            share_over0.5HT_total_goals = sum(total_HTgoals > 0) / length(btts),
            share_over1.5HT_total_goals = sum(total_HTgoals > 1) / length(btts),
            share_over2.5HT_total_goals = sum(total_HTgoals > 2) / length(btts),
            share_over0.5FT_total_goals = sum(total_FTgoals > 0) / length(btts),
            share_over1.5FT_total_goals = sum(total_FTgoals > 1) / length(btts),
            share_over2.5FT_total_goals = sum(total_FTgoals > 2) / length(btts),
            share_over3.5FT_total_goals = sum(total_FTgoals > 3) / length(btts),
            share_over0.5secondhalf_total_goals = sum(total_FTgoals - total_HTgoals > 0) / length(btts),
            share_over1.5secondhalf_total_goals = sum(total_FTgoals - total_HTgoals > 1) / length(btts),
            share_over0.5FT_team_goals = sum(FTHG > 0) /length(btts),
            share_over1.5FT_team_goals = sum(FTHG > 1) /length(btts),
            share_over2.5FT_team_goals = sum(FTHG > 2) /length(btts))

data_cards <- data_full %>% 
  group_by(Div, HomeTeam) %>% 
  summarize(team_cards_per_game = sum(c(HY, HR)) / length(btts),
            share_over0.5_team_cards = sum(c(HY, HR) > 0) / length(btts),
            share_over1.5_team_cards = sum(c(HY, HR) > 1) / length(btts),
            share_over2.5_team_cards = sum(c(HY, HR) > 2) / length(btts),
            share_team_red_card = sum(HR > 0) / length(btts),
            total_cards_per_game = sum(total_cards) / length(btts),
            share_over0.5_total_cards = sum(total_cards > 0) / length(btts),
            share_over1.5_total_cards = sum(total_cards > 1) / length(btts),
            share_over2.5_total_cards = sum(total_cards > 2) / length(btts),
            share_over3.5_total_cards = sum(total_cards > 3) / length(btts),
            share_over4.5_total_cards = sum(total_cards > 4) / length(btts),
            share_over5.5_total_cards = sum(total_cards > 5) / length(btts))

data_corners <- data_full %>% 
  group_by(Div, HomeTeam) %>% 
  summarize(team_corners_per_game = sum(HC) / length(btts),
            share_over0.5_team_corners = sum(HC > 0) / length(btts),
            share_over1.5_team_corners = sum(HC > 1) / length(btts),
            share_over2.5_team_corners = sum(HC > 2) / length(btts),
            share_over3.5_team_corners = sum(HC > 3) / length(btts),
            share_over4.5_team_corners = sum(HC > 4) / length(btts),
            share_over5.5_team_corners = sum(HC > 5) / length(btts),
            share_over6.5_team_corners = sum(HC > 6) / length(btts),
            share_over7.5_team_corners = sum(HC > 7) / length(btts),
            share_over8.5_team_corners = sum(HC > 8) / length(btts),
            total_corners_per_game = sum(total_corners) / length(btts),
            share_over0.5_total_corners = sum(total_corners > 0) / length(btts),
            share_over1.5_total_corners = sum(total_corners > 1) / length(btts),
            share_over2.5_total_corners = sum(total_corners > 2) / length(btts),
            share_over3.5_total_corners = sum(total_corners > 3) / length(btts),
            share_over4.5_total_corners = sum(total_corners > 4) / length(btts),
            share_over5.5_total_corners = sum(total_corners > 5) / length(btts),
            share_over6.5_total_corners = sum(total_corners > 6) / length(btts),
            share_over7.5_total_corners = sum(total_corners > 7) / length(btts),
            share_over8.5_total_corners = sum(total_corners > 8) / length(btts),
            share_over9.5_total_corners = sum(total_corners > 9) / length(btts),
            share_over10.5_total_corners = sum(total_corners > 10) / length(btts),
            share_over11.5_total_corners = sum(total_corners > 11) / length(btts),
            share_over12.5_total_corners = sum(total_corners > 12) / length(btts))