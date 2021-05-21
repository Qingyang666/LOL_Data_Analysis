
# load in the package 
library(tidyverse)
library(lubridate)

#####################DATA INTEGRATION & FEATURE ENGINEERING#####################

# read in the data
team_data_no_na <- read_csv("data/team_statistic.csv")
player_data_no_na <- read_csv("data/player_statistic.csv")

# create a function to condense player_statistic
player_data_condense <- function(var){
  player_data_no_na %>%
    select(gameid, side, position, var) %>%
    spread(position, var, sep = paste0("_",var,"_"))
}

# extract column names that need to spread
var_names <- colnames(player_data_no_na)[-c(1,2,3,4,5,6,7,8,10,11,15,16)]
# create a player_condensed_data tibble
player_condensed_data = player_data_condense('kills')[,c(1,2)]
for (i in 1:49) {
  player_condensed_data = cbind(player_condensed_data,
                                player_data_condense(var_names[i])[,3:7])
}

# remove prefix "position_" for all column names
colnames(player_condensed_data)<-gsub("position_", "", colnames(player_condensed_data))

# mix player_condensed_data with team data sets together to create complete_data 
complete_data <- team_data_no_na %>%
  left_join(y = player_condensed_data, by = c("gameid", "side"))

# Split team_stats_final into red_stats and blue_stats
blue_data <-  complete_data%>%
  filter(side == "Blue")

red_data <-  complete_data %>%
  filter(side == "Red")

# condense all statistic(team statistic + player statistic) for one match into single row
game_statistic <- blue_data %>%
  rename_all(list(~paste0(., ".blue"))) %>%
  left_join(y = red_data %>% rename_all( list(~paste0(., ".red"))),
            by = c("gameid.blue" = "gameid.red", 
                   "league.blue" = "league.red", 
                   "year.blue" = "year.red", 
                   "date.blue" = "date.red", 
                   "game.blue" = "game.red", 
                   "patch.blue" = "patch.red")) 

# rename several columns in game_statistic
game_statistic <- game_statistic %>%
  rename_at(.vars = vars(c("gameid.blue", "league.blue", "year.blue", "date.blue", "game.blue", "patch.blue")),
            list(~str_replace(.,".blue", "")))

# using date to create three specific variables: month, weekday, time.period
game_statistic <- game_statistic %>%
  mutate(month = month(date),
         weekdays = weekdays(date),
         time.period = case_when(
           hour(date) < 4 ~ '1',
           hour(date) >= 4 & hour(date) < 8 ~ '2',
           hour(date) >= 8 & hour(date) < 12 ~ '3',
           hour(date) >= 12 & hour(date) < 16 ~ '4',
           hour(date) >= 16 & hour(date) < 20 ~ '5',
           hour(date) >= 20 ~ '6')) %>%
  relocate(month, .after = year) %>%
  relocate(weekdays, .after = month) %>%
  relocate(time.period, .after = weekdays) %>% 
  select(-date)

# 对gamelength进行分箱，把游戏时长分成结束时间在20分钟以内，20-25分钟，25-30分钟，30分钟以上四组
game_statistic <- game_statistic %>%
  mutate(gamelength.blue = gamelength.blue/60,
         gamelength = case_when(gamelength.blue < 20 ~ 1,
                                gamelength.blue >= 20 & gamelength.blue < 25 ~ 2,
                                gamelength.blue >= 25 & gamelength.blue < 30 ~ 3,
                                gamelength.blue >= 30 ~ 4)) %>%
  relocate(gamelength, .after=gamelength.blue) %>%
  select(-gamelength.blue, -gamelength.red)

# result: 1 represents blue wins the match; 0 represents red wins the match
game_statistic <- game_statistic %>%
  relocate(result.red, .after = result.blue) %>%
  mutate(result = result.blue) %>%
  relocate(result, .before = gameid) %>%
  select(-result.blue, -result.red)

# based on team kills, assists, deaths at different minutes of game:
# generate k/d ratio = k/d
# kda ratio = (k+a)/d
# dr(dominance ratio: kills count as 2 points, deaths count as -3 points) = (2k+a)/3d
# where d is min(d, 1)
game_statistic <- game_statistic %>%
         # generate k/d ratio for blue team
  mutate(kd.blue = case_when(deaths.blue == 0 ~ round(kills.blue/(deaths.blue+1),3),
                             deaths.blue > 0 ~ round(kills.blue/deaths.blue,3)),
         # generate kda ratio for blue team
         kda.blue = case_when(deaths.blue == 0 ~ round((kills.blue+assists.blue)/(deaths.blue+1),3),
                              deaths.blue > 0 ~ round((kills.blue+assists.blue)/deaths.blue,3)),
         # generate dominance ratio for blue team
         dr.blue = case_when(deaths.blue == 0 ~ round((2*kills.blue+assists.blue)/(3*(deaths.blue+1)),3),
                             deaths.blue > 0 ~ round((2*kills.blue+assists.blue)/(3*deaths.blue),3)),
         
         # generate k/d ratio at 10 minutes for blue team
         kdat10.blue = case_when(deathsat10.blue == 0 ~ round(killsat10.blue/(deathsat10.blue+1),3),
                                 deathsat10.blue > 0 ~ round(killsat10.blue/deathsat10.blue,3)),
         # generate kda ratio at 10 minutes for blue team
         kdaat10.blue = case_when(deathsat10.blue == 0 ~ round((killsat10.blue+assistsat10.blue)/(deathsat10.blue+1),3),
                                  deathsat10.blue > 0 ~ round((killsat10.blue+assistsat10.blue)/deathsat10.blue,3)),
         # generate dominance ratio at 10 minutes for blue team
         drat10.blue = case_when(deathsat10.blue == 0 ~ round((2*killsat10.blue+assistsat10.blue)/(3*(deathsat10.blue+1)),3),
                                 deathsat10.blue > 0 ~ round((2*killsat10.blue+assistsat10.blue)/(3*deathsat10.blue),3)),
         
         # generate k/d ratio at 15 minutes for blue team
         kdat15.blue = case_when(deathsat15.blue == 0 ~ round(killsat15.blue/(deathsat15.blue+1),3),
                                 deathsat15.blue > 0 ~ round(killsat15.blue/deathsat15.blue,3)),
         # generate kda ratio at 15 minutes for blue team
         kdaat15.blue = case_when(deathsat15.blue == 0 ~ round((killsat15.blue+assistsat15.blue)/(deathsat15.blue+1),3),
                                  deathsat15.blue > 0 ~ round((killsat15.blue+assistsat15.blue)/deathsat15.blue,3)),
         # generate dominance ratio at 15 minutes for blue team
         drat15.blue = case_when(deathsat15.blue == 0 ~ round((2*killsat15.blue+assistsat15.blue)/(3*(deathsat15.blue+1)),3),
                                 deathsat15.blue > 0 ~ round((2*killsat15.blue+assistsat15.blue)/(3*deathsat15.blue),3)),
         
         # generate k/d ratio for red team
         kd.red = case_when(deaths.red == 0 ~ round(kills.red/(deaths.red+1),3),
                             deaths.red > 0 ~ round(kills.red/deaths.red,3)),
         # generate kda ratio for red team
         kda.red = case_when(deaths.red == 0 ~ round((kills.red+assists.red)/(deaths.red+1),3),
                              deaths.red > 0 ~ round((kills.red+assists.red)/deaths.red,3)),
         # generate dominance ratio for red team
         dr.red = case_when(deaths.red == 0 ~ round((2*kills.red+assists.red)/(3*(deaths.red+1)),3),
                            deaths.red > 0 ~ round((2*kills.red+assists.red)/(3*deaths.red),3)),
         
         # generate k/d ratio at 10 minutes for red team
         kdat10.red = case_when(deathsat10.red == 0 ~ round(killsat10.red/(deathsat10.red+1),3),
                                deathsat10.red > 0 ~ round(killsat10.red/deathsat10.red,3)),
         # generate kda ratio at 10 minutes for red team
         kdaat10.red = case_when(deathsat10.red == 0 ~ round((killsat10.red+assistsat10.red)/(deathsat10.red+1),3),
                                 deathsat10.red > 0 ~ round((killsat10.red+assistsat10.red)/deathsat10.red,3)),
         # generate dominance ratio at 10 minutes for red team
         drat10.red = case_when(deathsat10.red == 0 ~ round((2*killsat10.red+assistsat10.red)/(3*(deathsat10.red+1)),3),
                                deathsat10.red > 0 ~ round((2*killsat10.red+assistsat10.red)/(3*deathsat10.red),3)),
         
         # generate k/d ratio at 15 minutes for red team
         kdat15.red = case_when(deathsat15.red == 0 ~ round(killsat15.red/(deathsat15.red+1),3),
                                deathsat15.red > 0 ~ round(killsat15.red/deathsat15.red,3)),
         # generate kda ratio at 15 minutes for red team
         kdaat15.red = case_when(deathsat15.red == 0 ~ round((killsat15.red+assistsat15.red)/(deathsat15.red+1),3),
                                 deathsat15.red > 0 ~ round((killsat15.red+assistsat15.red)/deathsat15.red,3)),
         # generate dominance ratio at 15 minutes for red team
         drat15.red = case_when(deathsat15.red == 0 ~ round((2*killsat15.red+assistsat15.red)/(3*(deathsat15.red+1)),3),
                                deathsat15.red > 0 ~ round((2*killsat15.red+assistsat15.red)/(3*deathsat15.red),3))) %>%
  relocate(c("kd.blue", "kda.blue", "dr.blue"), .after = assists.blue) %>%
  relocate(c("kdat10.blue", "kdaat10.blue", "drat10.blue"), .after = deathsat10.blue) %>%
  relocate(c("kdat15.blue", "kdaat15.blue", "drat15.blue"), .after = deathsat15.blue) %>%
  relocate(c("kd.red", "kda.red", "dr.red"), .after = assists.red) %>%
  relocate(c("kdat10.red", "kdaat10.red", "drat10.red"), .after = deathsat10.red) %>%
  relocate(c("kdat15.red", "kdaat15.red", "drat15.red"), .after = deathsat15.red) 

# create k/d ratio difference for both side
# create kda ratio difference for both side
# create dominance ratio difference for both side
game_statistic <- game_statistic %>% 
  mutate(kddiff.blue = kd.blue - kd.red,
         kdadiff.blue = kda.blue-kda.red,
         drdiff.blue = dr.blue-dr.red) %>%
  relocate(c("kddiff.blue", "kdadiff.blue", "drdiff.blue"), .after = dr.blue) %>%
  mutate(kddiff.red = kd.red - kd.blue,
         kdadiff.red = kda.red-kda.blue,
         drdiff.red = dr.red-dr.blue) %>%
  relocate(c("kddiff.red", "kdadiff.red", "drdiff.red"), .after = dr.red) %>%
  mutate(kddiffat10.blue = kdat10.blue-kdat10.red,
         kdadiffat10.blue = kdaat10.blue-kdaat10.red,
         drdiffat10.blue = drat10.blue-drat10.red) %>% 
  relocate(c("kddiffat10.blue", "kdadiffat10.blue", "drdiffat10.blue"), .after = drat10.blue) %>%
  mutate(kddiffat10.red = kdat10.red-kdat10.blue,
         kdadiffat10.red = kdaat10.red-kdaat10.blue,
         drdiffat10.red = drat10.red-drat10.blue) %>% 
  relocate(c("kddiffat10.red", "kdadiffat10.red", "drdiffat10.red"), .after = drat10.red) %>%
  mutate(kddiffat15.blue = kdat15.blue-kdat15.red,
         kdadiffat15.blue = kdaat15.blue-kdaat15.red,
         drdiffat15.blue = drat15.blue-drat15.red) %>% 
  relocate(c("kddiffat15.blue", "kdadiffat15.blue", "drdiffat15.blue"), .after = drat15.blue) %>%
  mutate(kddiffat15.red = kdat15.red-kdat15.blue,
         kdadiffat15.red = kdaat15.red-kdaat15.blue,
         drdiffat15.red = drat15.red-drat15.blue) %>% 
  relocate(c("kddiffat15.red", "kdadiffat15.red", "drdiffat15.red"), .after = drat15.red)

# based on the player kills, assists and their team kills,
# generate kill participation rate for each player = (kills_i + assists_i)/(team_kills)
game_statistic <- game_statistic %>%
         # kill participation rate for each player on bottom lane in blue team
  mutate(kp_bot.blue = case_when(kills.blue == 0 ~ round((kills_bot.blue+assists_bot.blue)/(kills.blue+1),3),
                                 kills.blue > 0 ~ round((kills_bot.blue+assists_bot.blue)/kills.blue, 3)),
         # kill participation rate for each player on jungle lane in blue team   
         kp_jng.blue = case_when(kills.blue == 0 ~ round((kills_jng.blue+assists_jng.blue)/(kills.blue+1),3),
                                 kills.blue > 0 ~ round((kills_jng.blue+assists_jng.blue)/kills.blue, 3)),
         # kill participation rate for each player on middle lane in blue team
         kp_mid.blue = case_when(kills.blue == 0 ~ round((kills_mid.blue+assists_mid.blue)/(kills.blue+1),3),
                                 kills.blue > 0 ~ round((kills_mid.blue+assists_mid.blue)/kills.blue, 3)),
         # kill participation rate for each player on support lane in blue team
         kp_sup.blue = case_when(kills.blue == 0 ~ round((kills_sup.blue+assists_sup.blue)/(kills.blue+1),3),
                                 kills.blue > 0 ~ round((kills_sup.blue+assists_sup.blue)/kills.blue, 3)),
         # kill participation rate for each player on top lane in blue team
         kp_top.blue = case_when(kills.blue == 0 ~ round((kills_top.blue+assists_top.blue)/(kills.blue+1),3),
                                 kills.blue > 0 ~ round((kills_top.blue+assists_top.blue)/kills.blue, 3)),
         
         # kill participation rate for each player on bottom lane in blue team at 10 mins
         kpat10_bot.blue = case_when(killsat10.blue == 0 ~ round((killsat10_bot.blue+assistsat10_bot.blue)/(killsat10.blue+1),3),
                                     killsat10.blue > 0 ~ round((killsat10_bot.blue+assistsat10_bot.blue)/killsat10.blue, 3)),
         # kill participation rate for each player on jungle lane in blue team at 10 mins  
         kpat10_jng.blue = case_when(killsat10.blue == 0 ~ round((killsat10_jng.blue+assistsat10_jng.blue)/(killsat10.blue+1),3),
                                     killsat10.blue > 0 ~ round((killsat10_jng.blue+assistsat10_jng.blue)/killsat10.blue, 3)),
         # kill participation rate for each player on middle lane in blue team at 10 mins
         kpat10_mid.blue = case_when(killsat10.blue == 0 ~ round((killsat10_mid.blue+assistsat10_mid.blue)/(killsat10.blue+1),3),
                                     killsat10.blue > 0 ~ round((killsat10_mid.blue+assistsat10_mid.blue)/killsat10.blue, 3)),
         # kill participation rate for each player on support lane in blue team at 10 mins
         kpat10_sup.blue = case_when(killsat10.blue == 0 ~ round((killsat10_sup.blue+assistsat10_sup.blue)/(killsat10.blue+1),3),
                                     killsat10.blue > 0 ~ round((killsat10_sup.blue+assistsat10_sup.blue)/killsat10.blue, 3)),
         # kill participation rate for each player on top lane in blue team at 10 mins
         kpat10_top.blue = case_when(killsat10.blue == 0 ~ round((killsat10_top.blue+assistsat10_top.blue)/(killsat10.blue+1),3),
                                     killsat10.blue > 0 ~ round((killsat10_top.blue+assistsat10_top.blue)/killsat10.blue, 3)),
         
         # kill participation rate for each player on bottom lane in blue team at 15 mins
         kpat15_bot.blue = case_when(killsat15.blue == 0 ~ round((killsat15_bot.blue+assistsat15_bot.blue)/(killsat15.blue+1),3),
                                     killsat15.blue > 0 ~ round((killsat15_bot.blue+assistsat15_bot.blue)/killsat15.blue, 3)),
         # kill participation rate for each player on jungle lane in blue team at 15 mins  
         kpat15_jng.blue = case_when(killsat15.blue == 0 ~ round((killsat15_jng.blue+assistsat15_jng.blue)/(killsat15.blue+1),3),
                                     killsat15.blue > 0 ~ round((killsat15_jng.blue+assistsat15_jng.blue)/killsat15.blue, 3)),
         # kill participation rate for each player on middle lane in blue team at 15 mins
         kpat15_mid.blue = case_when(killsat15.blue == 0 ~ round((killsat15_mid.blue+assistsat15_mid.blue)/(killsat15.blue+1),3),
                                     killsat15.blue > 0 ~ round((killsat15_mid.blue+assistsat15_mid.blue)/killsat15.blue, 3)),
         # kill participation rate for each player on support lane in blue team at 15 mins
         kpat15_sup.blue = case_when(killsat15.blue == 0 ~ round((killsat15_sup.blue+assistsat15_sup.blue)/(killsat15.blue+1),3),
                                     killsat15.blue > 0 ~ round((killsat15_sup.blue+assistsat15_sup.blue)/killsat15.blue, 3)),
         # kill participation rate for each player on top lane in blue team at 15 mins
         kpat15_top.blue = case_when(killsat15.blue == 0 ~ round((killsat15_top.blue+assistsat15_top.blue)/(killsat15.blue+1),3),
                                     killsat15.blue > 0 ~ round((killsat15_top.blue+assistsat15_top.blue)/killsat15.blue, 3)),
         
         kp_bot.red = case_when(kills.red == 0 ~ round((kills_bot.red+assists_bot.red)/(kills.red+1),3),
                                kills.red > 0 ~ round((kills_bot.red+assists_bot.red)/kills.red, 3)),
         # kill participation rate for each player on jungle lane in red team   
         kp_jng.red = case_when(kills.red == 0 ~ round((kills_jng.red+assists_jng.red)/(kills.red+1),3),
                                kills.red > 0 ~ round((kills_jng.red+assists_jng.red)/kills.red, 3)),
         # kill participation rate for each player on middle lane in red team
         kp_mid.red = case_when(kills.red == 0 ~ round((kills_mid.red+assists_mid.red)/(kills.red+1),3),
                                kills.red > 0 ~ round((kills_mid.red+assists_mid.red)/kills.red, 3)),
         # kill participation rate for each player on support lane in red team
         kp_sup.red = case_when(kills.red == 0 ~ round((kills_sup.red+assists_sup.red)/(kills.red+1),3),
                                kills.red > 0 ~ round((kills_sup.red+assists_sup.red)/kills.red, 3)),
         # kill participation rate for each player on top lane in red team
         kp_top.red = case_when(kills.red == 0 ~ round((kills_top.red+assists_top.red)/(kills.red+1),3),
                                kills.red > 0 ~ round((kills_top.red+assists_top.red)/kills.red, 3)),
         
         # kill participation rate for each player on bottom lane in red team at 10 mins
         kpat10_bot.red = case_when(killsat10.red == 0 ~ round((killsat10_bot.red+assistsat10_bot.red)/(killsat10.red+1),3),
                                    killsat10.red > 0 ~ round((killsat10_bot.red+assistsat10_bot.red)/killsat10.red, 3)),
         # kill participation rate for each player on jungle lane in red team at 10 mins  
         kpat10_jng.red = case_when(killsat10.red == 0 ~ round((killsat10_jng.red+assistsat10_jng.red)/(killsat10.red+1),3),
                                    killsat10.red > 0 ~ round((killsat10_jng.red+assistsat10_jng.red)/killsat10.red, 3)),
         # kill participation rate for each player on middle lane in red team at 10 mins
         kpat10_mid.red = case_when(killsat10.red == 0 ~ round((killsat10_mid.red+assistsat10_mid.red)/(killsat10.red+1),3),
                                    killsat10.red > 0 ~ round((killsat10_mid.red+assistsat10_mid.red)/killsat10.red, 3)),
         # kill participation rate for each player on support lane in red team at 10 mins
         kpat10_sup.red = case_when(killsat10.red == 0 ~ round((killsat10_sup.red+assistsat10_sup.red)/(killsat10.red+1),3),
                                    killsat10.red > 0 ~ round((killsat10_sup.red+assistsat10_sup.red)/killsat10.red, 3)),
         # kill participation rate for each player on top lane in red team at 10 mins
         kpat10_top.red = case_when(killsat10.red == 0 ~ round((killsat10_top.red+assistsat10_top.red)/(killsat10.red+1),3),
                                    killsat10.red > 0 ~ round((killsat10_top.red+assistsat10_top.red)/killsat10.red, 3)),
         
         # kill participation rate for each player on bottom lane in red team at 15 mins
         kpat15_bot.red = case_when(killsat15.red == 0 ~ round((killsat15_bot.red+assistsat15_bot.red)/(killsat15.red+1),3),
                                    killsat15.red > 0 ~ round((killsat15_bot.red+assistsat15_bot.red)/killsat15.red, 3)),
         # kill participation rate for each player on jungle lane in red team at 15 mins  
         kpat15_jng.red = case_when(killsat15.red == 0 ~ round((killsat15_jng.red+assistsat15_jng.red)/(killsat15.red+1),3),
                                    killsat15.red > 0 ~ round((killsat15_jng.red+assistsat15_jng.red)/killsat15.red, 3)),
         # kill participation rate for each player on middle lane in red team at 15 mins
         kpat15_mid.red = case_when(killsat15.red == 0 ~ round((killsat15_mid.red+assistsat15_mid.red)/(killsat15.red+1),3),
                                    killsat15.red > 0 ~ round((killsat15_mid.red+assistsat15_mid.red)/killsat15.red, 3)),
         # kill participation rate for each player on support lane in red team at 15 mins
         kpat15_sup.red = case_when(killsat15.red == 0 ~ round((killsat15_sup.red+assistsat15_sup.red)/(killsat15.red+1),3),
                                    killsat15.red > 0 ~ round((killsat15_sup.red+assistsat15_sup.red)/killsat15.red, 3)),
         # kill participation rate for each player on top lane in red team at 15 mins
         kpat15_top.red = case_when(killsat15.red == 0 ~ round((killsat15_top.red+assistsat15_top.red)/(killsat15.red+1),3),
                                     killsat15.red > 0 ~ round((killsat15_top.red+assistsat15_top.red)/killsat15.red, 3))) %>%
  relocate(c("kp_bot.blue", "kp_jng.blue", "kp_mid.blue", "kp_sup.blue", "kp_top.blue"), .after = assists_top.blue) %>%
  relocate(c("kpat10_bot.blue", "kpat10_jng.blue", "kpat10_mid.blue", "kpat10_sup.blue", "kpat10_top.blue"), .after = deathsat10_top.blue) %>%
  relocate(c("kpat15_bot.blue", "kpat15_jng.blue", "kpat15_mid.blue", "kpat15_sup.blue", "kpat15_top.blue"), .after = deathsat15_top.blue) %>%
  relocate(c("kp_bot.red", "kp_jng.red", "kp_mid.red", "kp_sup.red", "kp_top.red"), .after = assists_top.red) %>%
  relocate(c("kpat10_bot.red", "kpat10_jng.red", "kpat10_mid.red", "kpat10_sup.red", "kpat10_top.red"), .after = deathsat10_top.red) %>%
  relocate(c("kpat15_bot.red", "kpat15_jng.red", "kpat15_mid.red", "kpat15_sup.red", "kpat15_top.red"), .after = deathsat15_top.red)

# create k/d ratio difference for each position of both side
# create kda ratio difference for each position of both side
# create dominance ratio difference for each position of both side
game_statistic <- game_statistic %>%
  mutate(kpdiff_bot.blue = kp_bot.blue-kp_bot.red,
         kpdiff_jng.blue = kp_jng.blue-kp_jng.red,
         kpdiff_mid.blue = kp_mid.blue-kp_mid.red,
         kpdiff_sup.blue = kp_sup.blue-kp_sup.red,
         kpdiff_top.blue = kp_top.blue-kp_top.red) %>%
  relocate(c("kpdiff_bot.blue", "kpdiff_jng.blue", "kpdiff_mid.blue", "kpdiff_sup.blue", "kpdiff_top.blue"), .after = kp_top.blue) %>%
  mutate(kpdiff_bot.red = kp_bot.red-kp_bot.blue,
         kpdiff_jng.red = kp_jng.red-kp_jng.blue,
         kpdiff_mid.red = kp_mid.red-kp_mid.blue,
         kpdiff_sup.red = kp_sup.red-kp_sup.blue,
         kpdiff_top.red = kp_top.red-kp_top.blue) %>%
  relocate(c("kpdiff_bot.red", "kpdiff_jng.red", "kpdiff_mid.red", "kpdiff_sup.red", "kpdiff_top.red"), .after = kp_top.red) %>%
  mutate(kpdiffat10_bot.blue = kpat10_bot.blue-kpat10_bot.red,
         kpdiffat10_jng.blue = kpat10_jng.blue-kpat10_jng.red,
         kpdiffat10_mid.blue = kpat10_mid.blue-kpat10_mid.red,
         kpdiffat10_sup.blue = kpat10_sup.blue-kpat10_sup.red,
         kpdiffat10_top.blue = kpat10_top.blue-kpat10_top.red) %>%
  relocate(c("kpdiffat10_bot.blue", "kpdiffat10_jng.blue", "kpdiffat10_mid.blue", "kpdiffat10_sup.blue", "kpdiffat10_top.blue"), .after = kpat10_top.blue) %>%
  mutate(kpdiffat10_bot.red = kpat10_bot.red-kpat10_bot.blue,
         kpdiffat10_jng.red = kpat10_jng.red-kpat10_jng.blue,
         kpdiffat10_mid.red = kpat10_mid.red-kpat10_mid.blue,
         kpdiffat10_sup.red = kpat10_sup.red-kpat10_sup.blue,
         kpdiffat10_top.red = kpat10_top.red-kpat10_top.blue) %>%
  relocate(c("kpdiffat10_bot.red", "kpdiffat10_jng.red", "kpdiffat10_mid.red", "kpdiffat10_sup.red", "kpdiffat10_top.red"), .after = kpat10_top.red) %>%
  mutate(kpdiffat15_bot.blue = kpat15_bot.blue-kpat15_bot.red,
         kpdiffat15_jng.blue = kpat15_jng.blue-kpat15_jng.red,
         kpdiffat15_mid.blue = kpat15_mid.blue-kpat15_mid.red,
         kpdiffat15_sup.blue = kpat15_sup.blue-kpat15_sup.red,
         kpdiffat15_top.blue = kpat15_top.blue-kpat15_top.red) %>%
  relocate(c("kpdiffat15_bot.blue", "kpdiffat15_jng.blue", "kpdiffat15_mid.blue", "kpdiffat15_sup.blue", "kpdiffat15_top.blue"), .after = kpat15_top.blue) %>%
  mutate(kpdiffat15_bot.red = kpat15_bot.red-kpat15_bot.blue,
         kpdiffat15_jng.red = kpat15_jng.red-kpat15_jng.blue,
         kpdiffat15_mid.red = kpat15_mid.red-kpat15_mid.blue,
         kpdiffat15_sup.red = kpat15_sup.red-kpat15_sup.blue,
         kpdiffat15_top.red = kpat15_top.red-kpat15_top.blue) %>%
  relocate(c("kpdiffat15_bot.red", "kpdiffat15_jng.red", "kpdiffat15_mid.red", "kpdiffat15_sup.red", "kpdiffat15_top.red"), .after = kpat15_top.red)

# this tibble contains team statistic and player statistic, one match one row.
write_csv(game_statistic, 'data/game_statistic.csv')