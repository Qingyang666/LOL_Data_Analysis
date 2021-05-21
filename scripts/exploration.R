library(tidyverse)
library(ggplot2)
library(gridExtra)

############################EXPLORATION#########################################

# read in team_statistc and player_statistic
team_statistic <- read_csv('data/team_statistic.csv')
player_statistic <- read_csv('data/player_statistic.csv')

#############################EXPLORE GAME LENGTH################################

# create a game length tibble in minutes group by different years
game_length_tbl <- team_statistic %>%
  group_by(year, gameid) %>%
  mutate(year = factor(year)) %>%
  summarize(minutes = gamelength/60) %>%
  distinct(gameid, .keep_all = TRUE) %>%
  ungroup() %>%
  select(-gameid) 
# visualization 1: game length in minutes density plot
theme_set(theme_minimal())
plot_gl1 <- ggplot(data = game_length_tbl,
                   aes(x = minutes, fill = year, color = year)) +
  geom_density(alpha = 0.2) +
  scale_x_continuous(name="Game Length (in minutes)", breaks = seq(0, 100, 10)) +
  scale_y_continuous(name = "Density", labels=scales::percent) +
  theme(legend.position = c(0.87, 0.70),
        legend.background = element_rect(fill = "white", color = "black"),
        legend.key.size = unit(0.3, 'cm'),
        axis.title.x = element_text(size=8,
                                    face="bold",
                                    family="Times"),
        axis.title.y = element_text(size = 8,
                                    face="bold",
                                    family="Times"))
# Commend:
# Based on the density plot of each year, the average game length tends to decrease as year increases. 
# This finding reflects that the pace of professional league of legends match is becoming faster.

# visualization 2 : game length ECDF plot
plot_gl2 <- ggplot(data = game_length_tbl,
                   aes(x = minutes)) + 
  stat_ecdf(geom = "point", color = "steelblue", size = 0.5) +
  scale_x_continuous(name="Game Length (in minutes)", breaks = seq(0, 100, 5))+
  scale_y_continuous(name= "Percentage of All Games", labels=scales::percent) +
  theme(axis.title.x = element_text(size=8,
                                    face="bold",
                                    family="Times"),
        axis.title.y = element_text(size = 8,
                                    face="bold",
                                    family="Times"))
# Commend:
# The empirical Empirical Cumulative Distribution Plot shows us:
# 1. 25% of matches were approximately 28 minutes or less
# 2. 50% of matches were approximately 33 minutes or less
# 3. 75% of matches were approximately 37 minutes or less

# layout each plots above
plot_game_length <- grid.arrange(plot_gl1, plot_gl2, ncol = 1, nrow = 2)
# save the plot
ggsave(plot_game_length, file='figures/game_length_plot.png', height=6, width=8)

############################EXPLORE WINS########################################

# create number of wins by side tibble called game_wins_tbl
game_wins_tbl1 <- team_statistic %>%
  group_by(year, side) %>%
  summarize(total_wins = sum(result)) %>%
  ungroup()

# visualization1: wins over time by sides
plot_wins_over_time <- ggplot(data = game_wins_tbl1, 
       aes(x = year, y = total_wins, fill = side)) + 
  geom_bar(position="dodge", stat="identity", alpha =0.6)+
  scale_fill_manual("side", values = c("Blue" = "blue", "Red" = "red")) + 
  labs(title = "Winning Over Time",
       x = "Year",
       y = "Total Wins")+
  theme(plot.title = element_text(size=13, face="bold", family="Times", hjust=0.5),
        axis.title.x = element_text(size=10, face="bold", family="Times", color="black"),
        axis.title.y = element_text(size = 10, face="bold", family="Times", color="black"))
# save the plot
ggsave(plot_wins_over_time, file='figures/wins_over_time_plot.png', height=6, width=8)

# Commend:
# In general, blue team has more winnings than red team has in each year.

# create a blue net wins by year and league tibble called game_wins_tbl2
game_wins_tbl2 <- team_statistic %>%
  group_by(year, league, side) %>%
  summarise(total_wins = sum(result)) %>%
  mutate(blue_net_wins = total_wins[side == 'Blue']- total_wins[side == 'Red']) %>%
  distinct(blue_net_wins, .keep_all = TRUE) %>%
  select(-total_wins) %>%
  mutate(blue_results = case_when(blue_net_wins >= 0 ~ "positive",
                                  blue_net_wins < 0 ~ "negative")) %>%
  ungroup() 

# create a blue net wins plot function
plot_blue_net_wins <- ggplot(data = game_wins_tbl2,
       aes(x = league, y = blue_net_wins, fill = blue_results))+
  geom_bar(stat = "identity")+
  scale_fill_manual("", values = c("positive" = "blue", "negative" = "red")) + 
  scale_y_continuous(name = "", breaks = seq(-20,70,10))+
  labs(title = "Blue Side Net Wins By League & Year",
       x = "League",
       y = "Net Wins")+
  theme(plot.title = element_text(size=8, face="bold", family="Times", hjust=0.5),
        axis.title.x = element_text(size=6, face="bold", family="Times"),
        axis.title.y = element_text(size = 6, face="bold", family="Times", color="black"),
        axis.text.x = element_text(size = 3, angle = 90, hjust = 1),
        legend.position = 'bottom',
        legend.title = element_text(size=6, 
                                    color = "black", 
                                    face="bold"),
        legend.key.size = unit(0.3, 'cm'))+
  facet_wrap(~ year)
# save the plot
ggsave(plot_blue_net_wins, file='figures/blue_net_wins_plot.png', height=6, width=8)

##############################BAN RATE##########################################

# ban1
champ_ban1_tbl <- team_statistic %>% 
  group_by(year) %>%
  mutate(tot_match = n()/2) %>%
  ungroup() %>%
  group_by(ban1, year) %>%
  summarize(ban1_tot = n(),
            ban1_rate = ban1_tot/tot_match) %>%
  distinct(year, .keep_all = TRUE) %>%
  arrange(desc(ban1_rate), .by_group = TRUE)%>%
  ungroup()

# ban2
champ_ban2_tbl <-  team_statistic %>% 
  group_by(year) %>%
  mutate(tot_match = n()/2) %>%
  ungroup() %>%
  group_by(ban2, year) %>%
  summarize(ban2_tot = n(),
            ban2_rate = ban2_tot/tot_match) %>%
  distinct(year, .keep_all = TRUE) %>%
  ungroup()

# ban3
champ_ban3_tbl <- team_statistic %>% 
  group_by(year) %>%
  mutate(tot_match = n()/2) %>%
  ungroup() %>%
  group_by(ban3, year) %>%
  summarize(ban3_tot = n(),
            ban3_rate = ban3_tot/tot_match) %>%
  distinct(year, .keep_all = TRUE) %>%
  ungroup()

# ban4
champ_ban4_tbl <- team_statistic %>% 
  group_by(year) %>%
  mutate(tot_match = n()/2) %>%
  ungroup() %>%
  group_by(ban4, year) %>%
  summarize(ban4_tot = n(),
            ban4_rate = ban4_tot/tot_match) %>%
  distinct(year, .keep_all = TRUE) %>%
  ungroup()

# ban5 
champ_ban5_tbl <- team_statistic %>% 
  group_by(year) %>%
  mutate(tot_match = n()/2) %>%
  ungroup() %>%
  group_by(ban5, year) %>%
  summarize(ban5_tot = n(),
            ban5_rate = ban5_tot/tot_match) %>%
  distinct(year, .keep_all = TRUE) %>%
  ungroup()

# full_join above tibbles each by each
ban_rate_tbl1 <- full_join(champ_ban1_tbl, champ_ban2_tbl, by = c("ban1" = "ban2", "year"))
ban_rate_tbl2 <- full_join(ban_rate_tbl1, champ_ban3_tbl, by = c("ban1" = "ban3", "year"))
ban_rate_tbl3 <- full_join(ban_rate_tbl2, champ_ban4_tbl, by = c("ban1" = "ban4","year"))
ban_rate_tbl4 <- full_join(ban_rate_tbl3, champ_ban5_tbl, by = c("ban1" = "ban5", "year"))

# convert all missing values into 0
ban_rate_tbl4[is.na(ban_rate_tbl4)] = 0

# top ten champion ban rate tibble by year
ban_rate <-  ban_rate_tbl4 %>% 
  filter(ban1 != "unknown") %>%
  select(-ends_with("_tot"))%>%
  rowwise() %>%
  mutate(ban_rate_total = sum(c(ban1_rate, ban2_rate, ban3_rate, ban4_rate, ban5_rate)))%>%
  select(-c(ban1_rate, ban2_rate, ban3_rate, ban4_rate, ban5_rate)) %>%
  group_by(year) %>%
  arrange(desc(ban_rate_total),.by_group = TRUE) %>%
  slice_head(n = 10) %>%
  ungroup()

# create a top 10 ban rate plot function
plot_ban_rate <- function(num){
  ban_rate %>%
    filter(year == num) %>%
    ggplot(aes(x = reorder(ban1,-ban_rate_total), y = ban_rate_total))+
    geom_bar(stat = "identity", fill="coral2")+
    labs(title = paste0("top 10 ban rate champions in ", num),
         x = "champions",
         y = "ban rate")+
    theme(plot.title = element_text(size=10, 
                                    face="bold", 
                                    family="Times",
                                    hjust=0.5),
          axis.title.x = element_text(size=8,
                                      face="bold",
                                      family="Times"),
          axis.title.y = element_text(size = 8,
                                      face="bold",
                                      family="Times"),
          axis.text.x = element_text(size = 5,
                                     angle = 60,
                                     hjust = 1))
}
# ban rate plot for year 2016
ban_rate_2016 <- plot_ban_rate(2016)
# ban rate plot for year 2017
ban_rate_2017 <- plot_ban_rate(2017)
# ban rate plot for year 2018
ban_rate_2018 <- plot_ban_rate(2018)
# ban rate plot for year 2019
ban_rate_2019 <- plot_ban_rate(2019)
# ban rate plot for year 2020
ban_rate_2020 <- plot_ban_rate(2020)
# ban rate plot for year 2021
ban_rate_2021 <- plot_ban_rate(2021)
# arrange all plots together
ban_rate_plot <- grid.arrange(ban_rate_2016,ban_rate_2017,ban_rate_2018,ban_rate_2019,ban_rate_2020,ban_rate_2021,
                              ncol = 3, nrow = 2)
#save the plot
ggsave(ban_rate_plot, file='figures/top10_ban_rate_plot.png', height=6, width=8)

###############################PICK RATE########################################

champ_pick <- player_statistic %>%
  group_by(year) %>%
  mutate(total_match = n()/10) %>%
  ungroup() %>%
  group_by(year, position, champion) %>%
  summarize(total_pick = n(),
            pick_rate = total_pick/total_match) %>%
  distinct(year,position,champion, .keep_all = TRUE) %>%
  ungroup() %>%
  group_by(year, position) %>%
  arrange(desc(pick_rate),.by_group = TRUE) %>%
  slice_head(n = 5) %>%
  ungroup()

# create a champion pick rate plot function
plot_pick_rate <- function(year_num, position_name, color_num){
  champ_pick %>%
    filter(year == year_num) %>%
    filter(position == position_name) %>%
    ggplot(aes(x = reorder(champion,-pick_rate), 
               y = pick_rate)) +
    geom_bar(stat = "identity", fill = color_num) +
    labs(title = paste0("top 5 pick rate champions in ", year_num),
         subtitle = "position of champions: jungle",
         x = "champions",
         y = "pick rate")+
    theme(plot.title = element_text(size=10, 
                                    face="bold", 
                                    family="Times",
                                    hjust=0.5),
          plot.subtitle = element_text(size=9, 
                                       face="bold", 
                                       family="Times",
                                       hjust=0.5),
          axis.title.x = element_text(size=8,
                                      face="bold",
                                      family="Times"),
          axis.title.y = element_text(size =8,
                                      face="bold",
                                      family="Times"),
          axis.text.x = element_text(size = 5,
                                     angle = 60,
                                     hjust = 1))
  
}
# pick rate in 2016, position is jungle
pick_rate_jng_2016 <- plot_pick_rate(year_num = 2016, position_name = "jng", color_num = "dodgerblue4")
# pick rate in 2017, position is jungle
pick_rate_jng_2017 <- plot_pick_rate(year_num = 2017, position_name = "jng", color_num = "dodgerblue3")
# pick rate in 2018, position is jungle
pick_rate_jng_2018 <- plot_pick_rate(year_num = 2018, position_name = "jng", color_num = "dodgerblue2")
# pick rate in 2019, position is jungle
pick_rate_jng_2019 <- plot_pick_rate(year_num = 2019, position_name = "jng", color_num = "dodgerblue1")
# pick rate in 2020, position is jungle
pick_rate_jng_2020 <- plot_pick_rate(year_num = 2020, position_name = "jng", color_num = "dodgerblue")
# pick rate in 2021, position is jungle
pick_rate_jng_2021 <- plot_pick_rate(year_num = 2021, position_name = "jng", color_num = "deepskyblue")

# arrange all plots together
pick_rate_plot <- grid.arrange(pick_rate_jng_2016,pick_rate_jng_2017,pick_rate_jng_2018,
                               pick_rate_jng_2019,pick_rate_jng_2020,pick_rate_jng_2021,
                               ncol = 3, nrow = 2)
# save the plot
ggsave(pick_rate_plot, file='figures/top5_pick_rate_plot.png', height=6, width=8)


