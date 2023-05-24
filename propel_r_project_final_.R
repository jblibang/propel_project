#The NBA draft is annual event the NBA hosts to select the most promising basketball players in the world to join the league.
#In order to be draft eligible a player must be at least 19 years old during the calendar year of the draft, and be one year removed from their high school graduation. 
#The first four - five picks of the draft are determined by a weighted lottery amongst the teams that didn't make the playoffs with the worst record having the highest weighted odds of receiving the top overall pick. 
#All remaining picks are determined by their regular season record. 

#Loads proper tools + data to do analysis
library(tidyverse)
library(ggplot2)
library(ggrepel)
draft_history <- read_csv("draft_history.csv")

#Question how can I get drafted into the NBA?

#Should I go to college, go to high school, go overseas, or pursue some other route?
ggplot(draft_history, aes(organization_type)) + 
  geom_bar()+
  labs(title="Where have people gone historically to pursue their basketball education?", 
       x="Organization Type",
       y="Number of Players")

#But this data is looking at all time. What happened last year?
ggplot(subset(draft_history,season %in% "2022"), 
       aes(organization)) + 
  geom_bar()  +
  coord_flip() +
labs(title="Organizations of 2022 NBA Draftees", 
     x="Organization",
     y="Number of Players")

#Gives players who went to Duke and which team they were drafted too and the year
ggplot(subset(draft_history, organization %in% "Duke"), 
       aes(season, team_name, label = player_name)) + 
  geom_point(size = 1) +
  xlim(2015, 2022)+ 
  geom_text_repel(aes(label= player_name), size = 3, hjust = -.05) +
  labs(title="Duke Basketball Players drafted since 2015", 
       x="Season",
       y="Team drafted by")

#Gives draft position of all Duke players drafted since 2015
ggplot(subset(draft_history, organization %in% "Duke"), 
       aes(season, overall_pick, label = player_name)) + 
  geom_point(size = 1) +
  xlim(2015, 2022)+ 
  ylim(1,60)+
  geom_text_repel(aes(label= player_name), size = 3, hjust = -.05) +
  labs(title="Draft Position of Duke Basketball players", 
       x="Season",
       y="Draft Position")

#Gives Organization of players drafted first overall since 2000
ggplot(subset(draft_history, overall_pick %in% "1"), 
       aes(season, team_name, label = organization)) + 
  geom_point(size = 1) +
  xlim(2000, 2022)+ 
  geom_text_repel(aes(label= organization), size = 3, hjust = -.05) +
  labs(title="Organization of players drafted first overall since 2000", 
       x="Season",
       y="Team Drafted to")

#Gives which teams have drafted first overall the most often
ggplot(subset(draft_history, overall_pick %in% "1"), 
       aes(team_name)) + 
  geom_bar(size = 1) +
  coord_flip() +
  labs(title="Teams with most first overall picks", 
       x="Team",
       y="Number of Picks")

#Which organizations have the 1st overall pick come from.
ggplot(subset(draft_history, overall_pick %in% "1"), 
       aes(organization)) + 
  geom_bar(size = 1) +
  scale_x_discrete(guide=guide_axis(angle = 90)) +
  labs(title="Teams with most first overall picks", 
       x="Team",
       y="Number of 1st Overall Picks")


#This gives the 1st round draft pick position of every team since 2014
ggplot(subset(draft_history, season > 2014), aes(team_name, overall_pick)) +
  geom_violin() +
  geom_dotplot(binaxis = 'y',
               stackdir= 'center',
               dotsize = .5) +
  ylim(1,30) +
  scale_x_discrete(guide=guide_axis(angle = 90)) +
  labs(title="NBA Team's 1st Round Draft Pick Selections since 2014", 
       subtitle="Average 1st Round Pick Position of Each Team",
       x="NBA Team",
       y="1st Round Draft Pick Position")

#1st round draft history of the Nets since 2014
ggplot(subset(draft_history, team_name %in% "Nets"), 
       aes(season, overall_pick, label = player_name)) + 
  geom_point(size = 1) +
  xlim(2014, 2022) +
  ylim(1,30) +
  geom_text_repel(aes(label= player_name), size = 3, hjust = -.05) +
  labs(title="Draft history of Nets since 2014", 
       x="Season",
       y="Draft Position")

#This gives the 1st round draft history of the Warriors during Stephen Curry's career
ggplot(subset(draft_history, team_name %in% "Warriors"), 
       aes(season, overall_pick, label = player_name)) + 
  geom_point(size = 1) +
  xlim(2008, 2022) +
  ylim(1,60) +
  geom_text_repel(aes(label= player_name), size = 3, hjust = -.05) +
  labs(title="Draft History of Golden State Warriors during Stephen Curry's career", 
       x="Season",
       y="Draft Position")

#Draft history of Warriors before draft Stephen Curry
ggplot(subset(draft_history, team_name %in% "Warriors"), 
       aes(season, overall_pick, label = player_name)) + 
  geom_point(size = 1) +
  xlim(1996, 2007) +
  ylim(1,60) +
  geom_text_repel(aes(label= player_name), size = 3, hjust = -.05) +
  labs(title="Draft History of Golden State Warriors before Stephen Curry", 
       x="Season",
       y="Draft Position")
