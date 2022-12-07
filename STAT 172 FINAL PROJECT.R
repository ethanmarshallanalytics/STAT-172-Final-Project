# STAT 172 Final Project
rm(list=ls())


# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("lubridate")
#install.packages("devtools")
# devtools::install_github("edwinth/paletti") 

library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(devtools)
library(paletti)

library(RColorBrewer)
library(randomForest)
library(pROC)
library(tidytext)
library(reshape2)
library(glmnet)

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
skater_stats <-read.csv(file.choose(), header = T)
# player_info.csv
player_info <-read.csv(file.choose(), header = T)


#adjusting data sets to get desired columns
skater_stats <- subset(skater_stats, select = -c(evenTimeOnIce, shortHandedTimeOnIce, powerPlayTimeOnIce))
player_info <- subset(player_info, select = c(player_id, firstName, lastName, nationality, primaryPosition, birthDate, height_cm, weight, shootsCatches))




#merging datasets
skater_stats <- left_join(skater_stats, player_info, by="player_id")

#reducing number of rows in data set by only selecting Boston Bruins Players
skater_stats <- filter(skater_stats, team_id == 6)

#removing extra columns
data <- subset(skater_stats, select = -c(game_id, player_id, team_id))


#fixing the birthDate column to age
data$birthDate <- substr(data$birthDate, 1,10)
data$birthDate <- as_date(data$birthDate)
data$age <- (2022 - year(data$birthDate))

#subsetting data to get final clean and organized data set
data <- subset(data, select = -c(birthDate, plusMinus, powerPlayGoals, shortHandedGoals))
data$score <- ifelse(data$goals >= 1, "Yes","No")
data
nrow(data)
str(data)
summary(data)

#have missing data in hits, takeaways, giveaways, and blocked, imputing median for all of them
data$hits[is.na(data$hits)] <-median(data$hits[!is.na(data$hits)])
data$takeaways[is.na(data$takeaways)] <-median(data$takeaways[!is.na(data$takeaways)])
data$giveaways[is.na(data$giveaways)] <-median(data$giveaways[!is.na(data$giveaways)])
data$blocked[is.na(data$blocked)] <-median(data$blocked[!is.na(data$blocked)])
data$shootsCatches[is.na(data$shootsCatches)] <- "L"

summary(data)
str(data)


### EXPLORATORY PLOTS ----
# Explanatory Graph with Shots
avg_by_position = data %>% 
  group_by(primaryPosition) %>%
  summarise(avg_goal = mean(goals)) %>% ungroup

ggplot() +
  geom_col(aes(x = primaryPosition, y = avg_goal), data = avg_by_position, color = "#010101", fill = "#FFB81C")+
  geom_boxplot(aes(x = primaryPosition, y = shots), data = data, color = "#010101", fill = "#FFB81C")+
  labs(x="Position", y="Shots/ Average Goals Scored") +
  ggtitle("Shots and Average Goals by Positiony") +
  scale_fill_manual(values=c("#010101", "#FFB81C"), "Goals \nScored")

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

### FINAL CLEANING ----
#Coverting character variables to factors to use for forest
data <- subset(data, select = -c(goals))
data$firstName <- as.factor(data$firstName)
data$lastName <- as.factor(data$lastName)
data$nationality <- as.factor(data$nationality)
data$primaryPosition <- as.factor(data$primaryPosition)
data$shootsCatches <- as.factor(data$shootsCatches)
data$score <- as.factor(data$score)
str(data)


#-------- Creating Training and testing sets and fitting the forest -----------
RNGkind(sample.kind = "default")
set.seed(3763)
train.idx <- sample(x=1:nrow(data), size=.7*nrow(data))
train.df <- data[train.idx, ]
test.df <- data[-train.idx, ]

str(train.df)
train.df <- subset(train.df, select=-c(firstName, lastName)) #remove first name and last name from train.df
str(train.df) 

forest1 <- randomForest(score ~ .,
                        data=train.df, #TRAINING DATA
                        ntree = 1000, #B = the number of classification trees in forest
                        mtry = 4, #choose m - sqrt(18) = 4.25 approx
                        importance = T)

forest1 # base OOB error = 20.97%
plot(forest1)

# tune mtry
mtry <- seq(1, 18, by=3) #TO DO: FIT FOR MORE M

# empty data frame for m and oob error
keeps <- data.frame(m=rep(NA, length(mtry)),
                    OOB_error_rate = rep(NA, length(mtry)))

# loop through different values of mtry
for(idx in 1:length(mtry)){
  print(paste0("Fitting m = ", mtry[idx]))
  tempforest <- randomForest(score ~.,
                             data=train.df,
                             ntree=1000,
                             mtry = mtry[idx], #mtry is varying
                             na.action = na.roughfix) #impute missings! otherwise get error
  #record iteration's m value in j'th row
  keeps[idx, "m"] <- mtry[idx]   #record OOB error, corresponding mtry for each forest fit
  keeps[idx, "OOB_error_rate"] <- mean(predict(tempforest) != train.df$score)
}
keeps # best OOB error = 20.53% at m=3

# plot the OOB error rates vs m
ggplot(data=keeps) +
  geom_line(aes(x=m, y=OOB_error_rate))
# best OOB error occurs at m=

# fit final forest

