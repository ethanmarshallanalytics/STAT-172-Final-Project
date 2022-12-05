# STAT 172 Final Project
rm(list=ls())

# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("lubridate")


library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

install.packages("devtools")

library(devtools)

devtools::install_github("edwinth/paletti") 
library(paletti)

mycols <- c(
  black    = "#010101",
  white = "#FFFFFF",
  gold   = "#FFB81C",
  orange = "#ffd289",
  beige = "#DDCBa4",
  brown = "#744F28"
  
)
viz_palette(mycols)

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

# Explanatory Graph with Shots
ggplot(data = data) +
  geom_histogram(aes(x=shots))
