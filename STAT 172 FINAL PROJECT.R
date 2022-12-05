# STAT 172 Final Project
rm(list=ls())


# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("lubridate")
# install.packages("devtools")
# devtools::install_github("edwinth/paletti") 

library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(devtools)
library(paletti)

# add Boston Bruins color palette
mycols <- c(
  black    = "#010101",
  white = "#FFFFFF",
  gold   = "#FFB81C",
  orange = "#ffd289",
  beige = "#DDCBa4",
  brown = "#744F28"
)
viz_palette(mycols) #view color palette

# use 'PLAYER DATA' zip file
# game_skater_stats.csv
skater_stats <-read.csv(choose.files(), header = T)
# player_info.csv
player_info <-read.csv(choose.files(), header = T)
### NOTE: if Mac user, use this code to download data
# skater_stats <-read.csv(file.choose(), header = T)
# player_info <-read.csv(file.choose(), header = T)


#adjusting data sets to get desired columns
skater_stats <- subset(skater_stats, select = -c(evenTimeOnIce, shortHandedTimeOnIce, powerPlayTimeOnIce))
player_info <- subset(player_info, select = c(player_id, firstName, lastName, nationality, primaryPosition, birthDate, height_cm, weight, shootsCatches))

skater_stats <- na.omit(skater_stats) # this might put some random attributes on column and cause problems for the random forest


#merging datasets
skater_stats <- left_join(skater_stats, player_info, by="player_id")

skater_stats <- na.omit(skater_stats)

#reducing number of rows in data set by only selecting 
skater_stats <- filter(skater_stats, team_id == 6)

data <- subset(skater_stats, select = -c(game_id, player_id, team_id))
data$birthDate <- substr(data$birthDate, 1,10)

#fixing the birthDate column to age
data$birthDate <- as_date(data$birthDate)
data$age <- (2022 - year(data$birthDate))

#subsetting data to get final clean and organized data set
data <- subset(data, select = -c(birthDate, plusMinus, powerPlayGoals, shortHandedGoals))
data$score <- ifelse(data$goals >= 1, "Yes","No")
data <- subset(data, select = -c(goals))
data
nrow(data)
str(data)

### EXPLORATORY PLOTS ----
# Explanatory Graph with Shots
avg_by_position = data %>% 
  group_by(primaryPosition) %>%
  summarise(avg_goal = mean(goals)) %>% ungroup

ggplot() +
  geom_col(aes(x = primaryPosition, y = avg_goal), data = avg_by_position)+
  geom_boxplot(aes(x = primaryPosition, y = shots), data = data)

# histogram of timeOnIce
ggplot(data=data) +
  geom_histogram(aes(x=timeOnIce), binwidth=75, color="#010101", fill="#FFB81C") +
  labs(x="Time on Ice (sec)", y="Frequency") +
  ggtitle("Distribution of Time on Ice")

# histogram of nationality and goals scored
ggplot(data=data) +
  geom_bar(aes(x=nationality, fill=score), position="fill") +
  labs(x="Nationality", y="Proportion") +
  ggtitle("Proportion of Goals Scored by Nationality") +
  scale_fill_manual(values=c("#010101", "#FFB81C"), "Goals \nScored")



