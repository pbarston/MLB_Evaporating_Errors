# need for data wrangling
library(tidyverse)
# need for plotting mlb team logos
library(mlbplotR)
# for getting mlb data
library(baseballr)
# need this for rotation and saving
library(grid)
# need this for labels
library(ggtext)
#need this to read the xcl
library(readxl)
#need this for scaling
library(scales)
#for themes
library(ggthemes)
#for the table
library(gt)
#for exporting
library(webshot2)
#for working with data
library(forcats)
#for nice labels
library(ggrepel)

# load data and remove 2020, seasons as factors
lg_fielding_standard = read.csv("/lg_fielding_standard.csv") %>% filter(Season != 2020) %>% mutate(Season = as.factor(Season))
lg_fielding_advanced = read.csv("/lg_fielding_advanced.csv") %>% filter(Season != 2020) %>% mutate(Season = as.factor(Season))
lg_hitting_statcast = read.csv("/lg_hitting_statcast.csv") %>% filter(Season != 2020) %>% mutate(Season = as.factor(Season))
lg_hitting_advanced = read.csv("/lg_hitting_advanced.csv") %>% filter(Season != 2020) %>% mutate(Season = as.factor(Season))
lg_hitting_batted_ball = read.csv("/lg_hitting_batted_ball.csv") %>% filter(Season != 2020) %>% mutate(Season = as.factor(Season))

#for reference:
# number of games in a season
tm_game_count = 162 * 30 #there are this many "team-games"
lg_game_count = tm_game_count / 2 #there are this many "play balls!"



#numerator will be errors from FG fielding leaderboards
#denominator will be BBE from FG hitting leaderboards

#plot  totals
errors_by_year_plot = lg_fielding_standard %>% ggplot(aes(x=Season,y=E,group = 1)) + geom_line() +
  #because we made season a factor, it thinks (correctly) we aren't trying to connect discrete observations
  #but we are, so group = 1 is just a dummy grouping
  theme_bw(base_size = 16) + #base font size, given in pts.
  labs(x = "Season",y = "Total Errors") +
  scale_y_continuous(label = comma) +
  geom_label_repel(aes(label = scales::comma(E)),color = "#e86100")+
  annotate("rect", xmin = 5.25, xmax = 5.75,
           ymin = 2505, ymax = 2900, fill= "#c2a5cf", alpha = .4, color = 'transparent') +
  #Categorical variables are simply placed at locations 1, 2, 3, etc. If you want to reach locations between two categorical variables, you can use coordinates such as 1.2 or 1.5 etc.
  annotate("text",
           x = 5.5,
           y = 2850,
           label = "COVID :(",
           family = "", fontface = 3, size=2)

errors_by_year_plot

#1b. plot it per game
errors_by_year_per_game_plot = lg_fielding_standard %>% 
  mutate(E_per_game = E / lg_game_count) %>%
  mutate(E_per_game = round(E_per_game,2)) %>% 
  ggplot(aes(x=Season,y=E_per_game,group = 1)) + geom_line() +
  #because we made season a factor, it thinks (correctly) we aren't trying to connect discrete observations
  #but we are, so group = 1 is just a dummy grouping
  theme_bw(base_size = 16) + #base font size, given in pts.
  labs(x = "Season",y = "Errors Per Game") +
  scale_y_continuous(label = comma) +
  geom_label_repel(aes(label = E_per_game),color = "#e86100")+
  annotate("rect", xmin = 5.25, xmax = 5.75,
           ymin = 1.04, ymax = 1.19, fill= "#c2a5cf", alpha = .4, color = 'transparent') +
  #Categorical variables are simply placed at locations 1, 2, 3, etc. If you want to reach locations between two categorical variables, you can use coordinates such as 1.2 or 1.5 etc.
  annotate("text",
           x = 5.5,
           y = 1.1,
           label = "COVID :(",
           family = "", fontface = 3, size=2)

errors_by_year_per_game_plot


#1c.  quick spin through other leagues? #correlation, etc...
#NPB
#independent leagues
#minor leagues
#winter leagues

#AAA Pacific Coast League errors / game
pcl_errors = data.frame(
  League = 'PCL',
  Season = as.factor(c('2015','2016','2017','2018','2019','2021','2022','2023','2024')),
  G = c(1151,1139,1124,1115,1116,642,746,747,747),
  E = c(1805,1692,1668,1541,1694,995,1094,1140,1133)
) %>% mutate(E_per_game = E / G)

#atlantic league errors / game
atlantic_league_errors = data.frame(
  League = 'ATL',
  Season = as.factor(c('2015','2016','2017','2018','2019','2021','2022','2023','2024')),
  G = c(557,560,559,504,558,479,659,623,627),
  E = c(1002,914,885,847,897,940,1184,967,1017)
) %>% mutate(E_per_game = E / G)

#KBO errors / game
kbo_errors = data.frame(
  League = 'KBO',
  Season = as.factor(c('2015','2016','2017','2018','2019','2021','2022','2023','2024')),
  G = c(720,720,720,720,720,720,720,720,720),
  E = c(1001,1045,982,994,996,1037,1130,1121,1088)
) %>% mutate(E_per_game = E / G)

#some plot comp
by_game_multi_league = lg_fielding_standard %>% 
  mutate(E_per_game = E / lg_game_count) %>%
  mutate(E_per_game = round(E_per_game,2)) %>% 
  mutate(League = 'MLB')%>%
  select(League,Season,G,E,E_per_game) %>%
  bind_rows(atlantic_league_errors,kbo_errors,pcl_errors)

multi_league_plot = by_game_multi_league %>%
  ggplot(aes(x = Season,y = E_per_game,group = League,colour = League)) + 
  geom_line(linewidth = 1.5) +
  theme_bw(base_size = 16) + #base font size, given in pts.
  labs(x = "Season",y = "Errors Per Game") +
  geom_vline(xintercept = 5,
             colour = 'black',
             linewidth = 0.5,
             linetype = 2)
multi_league_plot




# 2. per balls in play
#join them
errors_by_year_by_bbe = lg_fielding_standard %>% left_join(lg_hitting_statcast)

#select the relevant stuff
errors_by_year_by_bbe_slim = errors_by_year_by_bbe %>% select(c(Season,E,Events,FE,TE)) %>% mutate(E_per_1000_BBE = E / (Events/1000)) 

#plot it
errors_by_year_by_bbe_plot = errors_by_year_by_bbe_slim%>% mutate(E_per_1000_BBE = round(E_per_1000_BBE,2)) %>% 
  ggplot(aes(x=Season,y=E_per_1000_BBE,group ='dummy')) + geom_line() +
  #because we made season a factor, it thinks (correctly) we aren't trying to connect discrete observations
  #but we are, so group = 'dummy' is just a dummy grouping
  theme_bw(base_size = 16) + 
  labs(x = "Season",y = "Errors Per 1K BBE") +
  scale_y_continuous(labels = label_number(accuracy = 0.01),
                     n.breaks = 8) +
  geom_label_repel(aes(label = E_per_1000_BBE),color = "#e86100",size = 3)+
  annotate("rect", xmin = 5.25, xmax = 5.75,
           ymin = 20.25, ymax = 23, fill= "#c2a5cf", alpha = .4, color = 'transparent') +
  #Categorical variables are simply placed at locations 1, 2, 3, etc. If you want to reach locations between two categorical variables, you can use coordinates such as 1.2 or 1.5 etc.
  annotate("text",
           x = 5.5,
           y = 22.75,
           label = "COVID :(",
           family = "", fontface = 3, size=2)
errors_by_year_by_bbe_plot

#for reference
# number of BBE in a game:
BBE_per_game = data.frame(Season = errors_by_year_by_bbe_slim$Season,
                          BBE_per_game = errors_by_year_by_bbe_slim$Events / lg_game_count)
# roughly 50 BBE per play ball game, meaning 1K BBE is like 20 play ball games


#2a. how about ratio of FE to TE?
error_ratio_by_year_by_bbe_plot = errors_by_year_by_bbe_slim %>%  mutate(FE_to_TE = FE / TE) %>% 
  mutate(FE_to_TE = round(FE_to_TE,2)) %>% 
  ggplot(aes(x=Season,y=FE_to_TE,group ='dummy')) + geom_line() +
  #because we made season a factor, it thinks (correctly) we aren't trying to connect discrete observations
  #but we are, so group = 'dummy' is just a dummy grouping
  theme_bw(base_size = 16) + 
  labs(x = "Season",y = "Throwing Errors Per Fielding Error") +
  scale_y_continuous(labels = label_number(accuracy = 0.01),
                     n.breaks = 8) +
  geom_label_repel(aes(label = FE_to_TE),color = "#e86100",size = 3) +
  annotate("rect", xmin = 5.25, xmax = 5.75,
           ymin = 0.9, ymax = 1.225, fill= "#c2a5cf", alpha = .4, color = 'transparent') +
  #Categorical variables are simply placed at locations 1, 2, 3, etc. If you want to reach locations between two categorical variables, you can use coordinates such as 1.2 or 1.5 etc.
  annotate("text",
           x = 5.5,
           y = 1.2,
           label = "COVID :(",
           family = "", fontface = 3, size=2)
error_ratio_by_year_by_bbe_plot


#3. let’s facet wrap by position
#first, we need to download errors by position from FG, remove 2020, make seasons factors, make positions factors
lg_fielding_standard_c = read.csv("/lg_fielding_standard_c.csv") %>% filter(Season != 2020) %>% mutate(Season = as.factor(Season)) %>% mutate(Pos = as.factor(Pos))
lg_fielding_standard_1b = read.csv("/lg_fielding_standard_1b.csv") %>% filter(Season != 2020) %>% mutate(Season = as.factor(Season))%>% mutate(Pos = as.factor(Pos))
lg_fielding_standard_2b = read.csv("/lg_fielding_standard_2b.csv") %>% filter(Season != 2020) %>% mutate(Season = as.factor(Season))%>% mutate(Pos = as.factor(Pos))
lg_fielding_standard_ss = read.csv("/lg_fielding_standard_ss.csv") %>% filter(Season != 2020) %>% mutate(Season = as.factor(Season))%>% mutate(Pos = as.factor(Pos))
lg_fielding_standard_3b = read.csv("/lg_fielding_standard_3b.csv") %>% filter(Season != 2020) %>% mutate(Season = as.factor(Season))%>% mutate(Pos = as.factor(Pos))
lg_fielding_standard_lf = read.csv("/lg_fielding_standard_lf.csv") %>% filter(Season != 2020) %>% mutate(Season = as.factor(Season))%>% mutate(Pos = as.factor(Pos))
lg_fielding_standard_cf = read.csv("/lg_fielding_standard_cf.csv") %>% filter(Season != 2020) %>% mutate(Season = as.factor(Season))%>% mutate(Pos = as.factor(Pos))
lg_fielding_standard_rf = read.csv("/lg_fielding_standard_rf.csv") %>% filter(Season != 2020) %>% mutate(Season = as.factor(Season))%>% mutate(Pos = as.factor(Pos))
lg_fielding_standard_p = read.csv("/lg_fielding_standard_p.csv") %>% filter(Season != 2020) %>% mutate(Season = as.factor(Season))%>% mutate(Pos = as.factor(Pos))

#stack them
lg_fielding_standard_all_pos = bind_rows(lg_fielding_standard_c,
                                         lg_fielding_standard_1b,
                                         lg_fielding_standard_2b,
                                         lg_fielding_standard_ss,
                                         lg_fielding_standard_3b,
                                         lg_fielding_standard_lf,
                                         lg_fielding_standard_cf,
                                         lg_fielding_standard_rf,
                                         lg_fielding_standard_p
                                         )

#create simple total BBE by year
total_BBE_by_year = lg_hitting_statcast %>% select(Season,Events) %>% mutate(Season = as.factor(Season))
#join them in a new dataframe
errors_by_year_by_position_by_bbe = lg_fielding_standard_all_pos %>% left_join(total_BBE_by_year)
#select the relevant stuff
errors_by_year_by_position_by_bbe_slim = errors_by_year_by_position_by_bbe %>% select(c(Season,Pos,E,Events,FE,TE)) %>% 
  mutate(E_per_1000_BBE = E / (Events/1000)) %>%
  mutate(FE_to_TE = FE / TE)
#plot it
errors_by_year_by_position_by_bbe_plot = errors_by_year_by_position_by_bbe_slim %>% 
  mutate(E_per_1000_BBE = round(E_per_1000_BBE,2)) %>%
  ggplot(aes(x=Season,y=E_per_1000_BBE,group = 'dummy')) + geom_line() +
  geom_vline(xintercept = 5,
             colour = 'black',
             linewidth = 0.5,
             linetype = 2)+
  #because we made season a factor, it thinks (correctly) we aren't trying to connect discrete observations
  #but we are, so group = 'dummy' is just a dummy grouping
  labs(x = "Season",y = "Errors Per 1K BBE") + facet_wrap((~Pos),
                                                          nrow = 3,
                                                          ncol = 3, 
                                                          scales = "free_y",
                                                          axes = "all_y") + 
  theme_bw(base_size = 8) 
  

errors_by_year_by_position_by_bbe_plot

#3a. how about ratio of FE to TE?
#plot it
error_ratio_by_year_by_position_by_bbe_plot = errors_by_year_by_position_by_bbe_slim%>% mutate(FE_to_TE = round(FE_to_TE,2)) %>%
  ggplot(aes(x=Season,y=FE_to_TE,group = 'dummy')) + geom_line() +
  #because we made season a factor, it thinks (correctly) we aren't trying to connect discrete observations
  #but we are, so group = 'dummy' is just a dummy grouping
  labs(x = "Season",y = "Fielding Errors Per Throwing Error") + facet_wrap((~Pos),
                                                          nrow = 3,
                                                          ncol = 3, 
                                                          scales = "free_y",
                                                          axes = "all_y") + 
  theme_bw(base_size = 8) 


error_ratio_by_year_by_position_by_bbe_plot

#quick table to spot check!
errors_by_year_by_position_by_bbe_slim %>% group_by(Pos) %>% summarise(delta_error = E_per_1000_BBE[Season==2024] - E_per_1000_BBE[Season==2019])
