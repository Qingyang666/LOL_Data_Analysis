
library(tidyverse)
library(lubridate)
library(ggplot2)
library(tibble)
############################IMPOORT ALL DATASETS################################

champs <- read_csv('data/champs.csv')
itemsref <- read_csv('data/itemsref.csv')
summoner <- read_csv('data/summonerSpell.csv')
matches <- read_csv('data/matches.csv')
participants <- read_csv('data/participants.csv')
stat1 <- read_csv('data/stats1.csv')
stat2 <- read_csv('data/stats2.csv')
teambans <- read_csv('data/teambans.csv')
teamstats <- read_csv('data/teamstats.csv')

###############################PREPROCESSING####################################
#                                                                              #
#                     STEP1 GENERATE PLAYERS DATA SET                          #
################################################################################

champs <- champs %>% arrange(id)

summoner <- summoner %>% arrange(keys) 

itemsref <- itemsref %>% 
  select(keys, names) %>%
  # remove observation with NA
  filter(keys != 3632) %>% 
  # generate new one observation indicating no item purchased
  add_row(keys = 0, names = "No Item Purchased") %>%
  arrange(keys)
  
# convert summoner spell id into actual name
players_dat <- left_join(x = participants, y = summoner, by = c("ss1" = "keys"))
players_dat <- left_join(x = players_dat, y = summoner, by = c("ss2" = "keys"))
cat("Since there is no summoner spell id of 2 and 10, then the row id with missing values are", 
    which(is.na(players_dat) ==T))

# generate new column that indicates team role for each player
final_position <- c()
for(i in 1:nrow(players_dat)){
  if(players_dat$role[i] == "DUO_SUPPORT"){
    final_position[i] <- players_dat$role[i]
    } else if (players_dat$role[i] == "DUO_CARRY"){
      final_position[i] <- players_dat$role[i] 
      } else {
        final_position[i] <- players_dat$position[i]
      }
}
players_dat <- players_dat %>% 
  mutate(team_role = final_position) %>%
  relocate(team_role, .after = position) %>%
  rename(spell1 = Spells.x, spell2 = Spells.y) %>%
  relocate(c(spell1,spell2), .after = ss2)
         
# convert champion id into actual name
players_dat <- left_join(x = players_dat, y = champs, by = c("championid" = "id"))
players_dat <- players_dat %>%
  select(-championid) %>%
  rename(champion = name) %>%
  relocate(champion, .after = player)

# join matches data into participants data
players_dat <- left_join(x = players_dat, y = matches, by = c("matchid" = "id"))
players_dat <- players_dat %>%
  relocate(c(platformid, queueid, seasonid, gameid), .before = matchid)

###############################PREPROCESSING####################################
#                                                                              #
#               STEP2 GENERATE PLAYERS MATCH STATISTIC DATA SET                #
################################################################################

# bind two data sets by rows
stats <- bind_rows(stat1, stat2)

# convert item id into actual name 
stats <- left_join(x = stats, y = itemsref, by = c("item1" = "keys"))
stats <- left_join(x = stats, y = itemsref, by = c("item2" = "keys"))
stats <- left_join(x = stats, y = itemsref, by = c("item3" = "keys"))
stats <- left_join(x = stats, y = itemsref, by = c("item4" = "keys"))
stats <- left_join(x = stats, y = itemsref, by = c("item5" = "keys"))
stats <- left_join(x = stats, y = itemsref, by = c("item6" = "keys"))
stats <- stats %>%
  relocate(starts_with('names.'), .after = starts_with('item'))

# join stats data into players data called mydata1
mydata1 <- left_join(x = players_dat, y = stats, by = "id")

# check number of observations in each season
mydata1 %>% 
  group_by(seasonid)%>%
  summarize(num_obs = n())

# check proportion of nas in each column
proportion.nas <- mydata1 %>%
  group_by(seasonid) %>%
  summarize_all(funs(nas = sum(is.na(.))/dplyr::n()))

# drop all the observations with NA
mydata1 <- mydata1 %>% filter(complete.cases(mydata1))
cat('The proportion of missing values that I removed is', (1834520-1352659)/1834520)

# check how many matches without total players of 10
test <- mydata1 %>% 
  group_by(matchid) %>%
  summarize(num_obs = n())
cat('There are',length(which(test$num_obs != 10)),'matches with players that are not equal to 10.')

# filter matchid with total players of 10
test <- test %>%  
  filter(num_obs == 10) %>%
  select(-num_obs)

# generate player match statistic data set
mydata1 <- semi_join(x = mydata1, y = test, by = "matchid")

