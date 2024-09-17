setwd("~/Desktop/Miami Metrics")
library(tidyverse)

soccer_data <- read_csv("matches_1930_2022.csv") %>%
  select(home_team, away_team, home_score, away_score)

home_data <- soccer_data %>%
  select(Team=home_team, Score=home_score)

away_data <- soccer_data%>%
  select(Team=away_team, Score=away_score)

combined_data <- rbind(home_data, away_data)

agg_data <- combined_data%>%
  group_by(Team) %>%
  summarise(Sum_Goals = sum(Score))

fixed_data <- agg_data %>%
  mutate(Team = case_when(
    Team == "West Germany" ~ "Germany",
    Team == "China PR" ~ "China",
    Team == "United States" ~ "USA",
    Team == "IR Iran" ~ "Iran",
    Team == "England" ~ "UK",
    Team == "Germany DR" ~ "Germany",
    TRUE ~ Team
  ))

#install.packages("maps")
library(maps)
world <- map_data("world")

world_combined_data <- full_join(world, fixed_data, by=c("region" = "Team"))

ggplot(world_combined_data, aes(x=long, y=lat, group = group)) + 
  geom_polygon(aes(fill=Sum_Goals), color="gray80", linewidth = 0.1)+
  scale_fill_continuous(low = "light blue", high = "dark blue", na.value="gray90") + 
  theme_void()+
  labs(title = "Total Goals by Each Country in the World Cup (1930-2022)",
       caption = "Source: Petro via Kaggle",
       fill = "Goal Count") +
  theme(plot.title = element_text(hjust=0.5, size=15, face="bold"))

world_cup <- read_csv("world_cup.csv") %>%
  select(Champion)%>%
  mutate(Team = case_when(
    Team == "West Germany" ~ "Germany",
    Team == "China PR" ~ "China",
    Team == "United States" ~ "USA",
    Team == "IR Iran" ~ "Iran",
    Team == "England" ~ "UK",
    Team == "Germany DR" ~ "Germany",
    TRUE ~ Team
  ))

world_cup_agg <- world_cup %>%
  group_by(Team)%>%
  summarise(Num_wins = n())

world_cup_combined <- full_join(world_cup_agg, world_data, c("Team" = "region"))  

ggplot(world_cup_combined, aes(x=long, y=lat, group = group)) + 
  geom_polygon(aes(fill=Sum_Goals), color="gray80", linewidth = 0.1)+
  scale_y_continuous(loq = "light blue", high = "dark blue", na.value="gray90") + 
  theme_void()+
  labs(title = "Total Wins by Each Country in the World Cup (1930-2022)",
       caption = "Source: Petro via Kaggle",
       fill = "Win Count") +
  theme(plot.title = element_text(hjust=0.5, size=15, face="bold"))

ggsave(plot=p1, "Goals.png")
ggsave(plot=p2, "Wins.png")
