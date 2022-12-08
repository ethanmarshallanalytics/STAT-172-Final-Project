# STAT 172 Final Project
rm(list=ls())

# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("lubridate")
#install.packages("devtools")
# devtools::install_github("edwinth/paletti") 
devtools::install_github("thomasp85/patchwork")

library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(devtools)
library(paletti)
library(patchwork)
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

shots <- ggplot() +
  geom_boxplot(aes(x = primaryPosition, y = shots), data = data, color = "#010101", fill = "#FFB81C")+
  labs(x="Position", y="Shots per game") +
  ggtitle("Shots per Game by Position")

goals <- ggplot() +
  geom_col(aes(x = primaryPosition, y = avg_goal), data = avg_by_position, color = "#010101", fill = "#FFB81C")+
  labs(x="Position", y="Average Goals Scored") +
  ggtitle("Goals per Game by Position")

shots + goals

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

#-------- CREATE TRAINING AND TESTING SETS -----------
RNGkind(sample.kind = "default")
set.seed(3763)
train.idx <- sample(x=1:nrow(data), size=.7*nrow(data))
train.df <- data[train.idx, ]
test.df <- data[-train.idx, ]

str(train.df)
train.df <- subset(train.df, select=-c(firstName, lastName)) #remove first name and last name from train.df
str(train.df) 

#-------- FIT THE FOREST -----------
forest1 <- randomForest(score ~ .,
                        data=train.df, #TRAINING DATA
                        ntree = 1000, #B = the number of classification trees in forest
                        mtry = 4, #choose m - sqrt(18) = 4.25 approx
                        importance = T)

forest1 # base OOB error = 20.97%
plot(forest1)

# ---- TUNE FOREST -----------
# tune mtry
mtry <- c(1:18)

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
keeps # best OOB error = XX% at m=X

# plot the OOB error rates vs m
ggplot(data=keeps) +
  geom_line(aes(x=m, y=OOB_error_rate)) # TODO: !!!!! MAKE THIS LOOK NICER !!!!!
# best OOB error occurs at m=7

# fit final forest
forest2 <- randomForest(score ~.,
                        data=train.df, #TRAINING DATA
                        ntree = 1000,
                        mtry = 5,
                        importance = T,
                        na.action = na.roughfix)
forest2 # OOB error = 12.92%

# ---- PLOT ROC CURVE ---------------
# establish p-hat ... "Yes" is a positive event
pi_hat <- predict(forest2, test.df, type="prob")[,"Yes"]
# create curve
rocCurve <- roc(response = test.df$score, #supply truth (from test set)
                predictor = pi_hat, #supply predicted PROBABILITIES of positive case
                levels = c("No", "Yes")) #(negative, positive)
# plot basic ROC curve
plot(rocCurve, print.thres = TRUE, print.auc = TRUE)
# AUC = 0.816
# pi* = 0.132 ... we will only predict churn when P(scoring) > .132
# Specificity = 0.676 ... When a goal isn't scored, we correctly predict that a player
  # doesn't score 67.6% of the time.
# Sensitivity = 0.785 ... When a goal is scored, we correctly predict that a player
  # will score 78.5% of the time.

# create a column of predicted values in the test data
pi_star <- coords(rocCurve, "best", ret="threshold")$threshold[1]
test.df$score_pred <- as.factor(ifelse(pi_hat > pi_star, "Yes", "No"))
View(test.df)

# -------- FITTING A GLM -----
# make a variable importance plot
vi <- as.data.frame(varImpPlot(forest2, type = 1))
vi$Variable <- rownames(vi)
ggplot(data = vi) +
  geom_bar(aes(x = reorder(Variable,MeanDecreaseAccuracy), 
               weight = MeanDecreaseAccuracy), 
            position ="identity", color = "#010101", fill="#FFB81C") + 
            coord_flip() + 
            labs(x = "Variable Name", y = "Importance") +
            ggtitle("Variable Importance Plot for Predicting 'score'")

# USING VARIABLE IMPORTANCE PLOT
# add variables to find optimum model

# USING BACKWARDS REGRESSION

# fit logistic regression
glm2 = glm(score ~ shots + timeOnIce + primaryPosition + hits + age + 
             faceOffWins + powerPlayAssists, 
            data = data, family = binomial(link = "logit"))

glm3 = glm(score ~ shots + timeOnIce + primaryPosition + weight + hits + age + nationality, data=data, family = binomial(link = "logit"))
AIC(glm3)

AIC(glm2) # calculate the AIC of the model
BIC(glm2) # calculate the BIC of the model
summary(glm2)
# GLM3 is the final Bernoulli model using backwards regression. 

# ------- CLASSIFICATION TREE ------------
# tune a tree using the best GLM model and compare AUC to the forest






# ------ FINAL VISUALIZATIONS --------
# create visualizations using the most important variables












