
# load in the package 
library(tidyverse)
library(foreach)

###############################Data Cleaning####################################

# read in each data set from year 2016 to year 2021
lol_data <- foreach(year = 2016:2021, .combine='rbind') %do% {
  filename <- paste0('data/',year, '.csv')
  mydata <- read_csv(filename)
  mydata
}

# NOTE:
# In the full data set, player id with 1-5 represents individual player in blue team 
# and player id with 6-10 represents individual player in red team.
# In the full data set, position with blue represents blue team and position with red represents red team.

# extract team data set from lol_data called as team_data
team_data <- lol_data %>%
  filter(position == "team")

# show variable 'datacompleteness' 
table(team_data$datacompleteness)

# extract players data set from lol_data called as player_data
player_data <- lol_data %>%
  filter(position != "team")

# show variable 'datacompleteness'
table(player_data$datacompleteness)

## use only observations with complete information
# for team data set
team_data_complete <- team_data %>% 
  filter(datacompleteness == "complete")
cat('The proportion of observations that are removed from the team_data tibble is', 
    (nrow(team_data)-nrow(team_data_complete))/nrow(team_data))

# for player data set
player_data_complete <- player_data %>% 
  filter(datacompleteness == "complete")
cat('The proportion of observations that are removed from the player_data tibble is', 
    (nrow(player_data)-nrow(player_data_complete))/nrow(player_data))

# specify irrelevant columns in team_data_complete tibble
team_irrelevant <- c("datacompleteness", "url", "split", "playoffs", 
                     "playerid", "position", "team")

# specify redundant columns in team_data_complete tibble
team_redundant <- c("player", "champion", "teamkills", "teamdeaths", "firstbloodkill", 
                    "firstbloodassist", "firstbloodvictim", "dragons (type unknown)", 
                    "damageshare", "earnedgoldshare", "total cs")

# remove above specified columns 
# remove variables with "opp_" 
# due to the patch version, remove some variables that are not consistent
team_data_remove <- team_data_complete %>%
  select(-all_of(team_irrelevant),
         -all_of(team_redundant),
         -starts_with("opp_"),
         -damagemitigatedperminute,
         -visionscore,
         -vspm)

cat('The number of columns that we removed in team_data_complete is', 
    ncol(team_data_complete)-ncol(team_data_remove))

# specify irrelevant columns in player_data_complete tibble
player_irrelevant <- c("datacompleteness", "url", "split", "playoffs",  
                       "playerid", "player","team")

# specify redundant columns in player_data_complete tibble
player_redundant <- c("ban1", "ban2", "ban3", "ban4", "ban5", "firstblood", "firstdragon", 
                      "team kpm", "ckpm", "dragons", "opp_dragons", "elementaldrakes", 
                      "opp_elementaldrakes", "infernals", "mountains", "clouds", "oceans", 
                      "dragons (type unknown)", "elders", "opp_elders", "firstherald", 
                      "heralds", "opp_heralds", "firstbaron", "barons", "opp_barons", 
                      "firsttower", "towers", "opp_towers", "firstmidtower", "firsttothreetowers", 
                      "inhibitors", "opp_inhibitors", "gspd")

# remove above specified columns 
# remove variables with "opp_" 
# due to the patch version, remove some variables that are not consistent
player_data_remove <- player_data_complete %>%
  select(-all_of(player_irrelevant),
         -all_of(player_redundant),
         -starts_with("opp_"),
         -damagemitigatedperminute,
         -visionscore,
         -vspm)

cat('The number of columns that we removed in player_data_complete is', 
    ncol(player_data_complete)-ncol(player_data_remove))

# count the number of missing values in team_data_remove by each league
team_data_nas <- team_data_remove %>%
  group_by(league) %>%
  summarize_all(funs(nas = sum(is.na(.))))

# count the number of missing values in player_data_remove by each league
player_data_nas <- player_data_remove %>%
  group_by(league) %>%
  summarize_all(funs(nas = sum(is.na(.))))

# NOTE:
# After checking missing values, I found that the number of missing values in "minionkills" and 
# the number of missing values in "cspm" from the team_data_na tibble do not match with player_data_na tibble.
# Since each value in variable "minionkills" from the team_data_remove corresponds to the sum of the values that
# each side of player made per game from the variable "minionkills" in player_data_remove.

# create a total_minionkills tibble from player_data_remove called minonkills_by_team
minonkills_by_team <- player_data_remove %>%
  group_by(year, gameid, side) %>%
  summarise(minionkills_fix = sum(minionkills),
            cspm_fix = round((sum(minionkills)+sum(monsterkills))/(gamelength/60),4)) %>%
  distinct(gameid, .keep_all = TRUE) %>%
  ungroup()

# left join to fix the issue above
team_data_remove <- team_data_remove %>%
  left_join(minonkills_by_team, by = c("year","gameid", "side")) %>%
  relocate(minionkills_fix, .after = minionkills) %>%
  relocate(cspm_fix, .after=cspm) %>%
  select(-minionkills, -cspm) %>%
  rename(minionkills = minionkills_fix,
         cspm = cspm_fix)

# convert missing values from ban1 to ban5 into unknown
team_data_remove <- team_data_remove %>%
  mutate(ban1 = replace_na(ban1, "None"),
         ban2 = replace_na(ban2, "None"),
         ban3 = replace_na(ban3, "None"),
         ban4 = replace_na(ban4, "None"),
         ban5 = replace_na(ban5, "None"))

# recheck missing values in team_data_remove
team_data_nas <- team_data_remove %>%
  group_by(league) %>%
  summarize_all(funs(nas = sum(is.na(.))))

# remove all missing values 
team_data_no_na <- team_data_remove %>% drop_na()
cat('The number of observations that are removed due to missing values is', 
    (nrow(team_data_remove)-nrow(team_data_no_na))/nrow(team_data_remove))

player_data_no_na <- player_data_remove %>% drop_na()
cat('The number of observations that are removed due to missing values is', 
    (nrow(player_data_remove)-nrow(player_data_no_na))/nrow(player_data_remove))

# write out csv
write_csv(team_data_no_na,"data/team_statistic.csv")
write_csv(player_data_no_na, "data/player_statistic.csv")
