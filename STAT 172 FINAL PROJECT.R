# STAT 172 Final Project
rm(list=ls())

install.packages("dplyr")
install.packages("tidyr")
install.packages("lubridate")


library(dplyr)
library(tidyr)
library(lubridate)

<<<<<<< HEAD
skater_stats <-read.csv(choose.files(), header = T)
player_info <-read.csv(choose.files(), header = T)
=======
#reading in data sets:
skater_stats <-read.csv("Final_project/game_skater_stats.csv")
player_info <-read.csv("Final_project/player_info.csv")
>>>>>>> 8c5975ed31cb20fef7364f1c416bc6020c733243

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
data <- subset(data, select = -c(birthDate))
data <- subset(data, select = -c(powerPlayGoals, shortHandedGoals))
data$score <- ifelse(data$goals >= 1, "Yes","No")
data <- subset(data, select = -c(goals))
data
nrow(data)
str(data)
