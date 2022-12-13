# STAT 172 Final Project
rm(list=ls())

# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("lubridate")
#install.packages("devtools")
# devtools::install_github("edwinth/paletti") 
#devtools::install_github("thomasp85/patchwork")

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
library(MASS)

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
# Explanatory Graph with Shots and Goals
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

shots + goals # plots graphs together on the same plane

# histogram of timeOnIce
ggplot(data=data) +
  geom_histogram(aes(x=timeOnIce), binwidth=75, color="#010101", fill="#FFB81C") +
  labs(x="Time on Ice (sec)", y="Frequency") +
  ggtitle("Distribution of Time on Ice")

# bar chart of nationality and shots taken
avg_shots_by_pos = data %>% 
  group_by(nationality) %>%
  summarise(avg_shots = mean(shots)) %>% ungroup

shots_by_pos <- ggplot() +
  geom_col(aes(x = nationality, y = avg_shots), data =
             avg_shots_by_pos, color = "#010101", fill = "#FFB81C")+
  labs(x="Nationality", y="Average Shots Scored") +
  ggtitle("Shots per Game by Nationality")

shots_by_pos

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
                        mtry = 4, #choose m: sqrt(18) = 4.25 approx
                        importance = T)

forest1 # base OOB error = 13.01%
plot(forest1)

# ---- TUNE FOREST -----------
# tune mtry, the number of predictor variables used in the random forest
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
keeps # best OOB error = 12.92375% at m=5

# plot the OOB error rates vs m
ggplot(data=keeps) +
  geom_line(aes(x=m, y=OOB_error_rate)) + 
  geom_point(aes(x=m, y=OOB_error_rate)) +
  labs(x="m (mtry) value", y="Out of Bag error rate (OOB)") +
  theme_bw() + 
  scale_x_continuous(limits=c(1,18), breaks=seq(1,18,1)) +
  ylim(0.125,0.150) +
  ggtitle("Tuned Random Forest Performance Based on the Number of Predictor Variables (m)")
  

# grab the best mtry value automatically
optimal_m <- which.min(keeps[,"OOB_error_rate"])

# fit final forest
forest2 <- randomForest(score ~.,
                        data=train.df, #TRAINING DATA
                        ntree = 1000,
                        mtry = optimal_m,
                        importance = T,
                        na.action = na.roughfix)
forest2 # OOB error = 12.88%

# ---- PLOT ROC CURVE ---------------
# establish p-hat ... "Yes" is a positive event
pi_hat <- predict(forest2, test.df, type="prob")[,"Yes"]
# create curve
rocCurve <- roc(response = test.df$score, #supply truth (from test set)
                predictor = pi_hat, #supply predicted PROBABILITIES of positive case
                levels = c("No", "Yes")) #(negative, positive)
# plot basic ROC curve
plot(rocCurve, print.thres = TRUE, print.auc = TRUE)
# AUC = 0.817
# pi* = 0.144 ... we will only predict a goal when P(scoring) > .144
# Specificity = 0.704 ... When a goal isn't scored, we correctly predict that a player
  # doesn't score 70.4% of the time.
# Sensitivity = 0.761 ... When a goal is scored, we correctly predict that a player
  # will score 76.1% of the time.

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

### USING VARIABLE IMPORTANCE PLOT
# add variables to find optimum model (forward stepwise regression)
# fit logistic regression
glm1a = glm(score ~ shots + timeOnIce, 
           data=data, family = binomial(link = "logit"))
AIC(glm1a) # 23580.5
BIC(glm1a) # 23605.7


glm1b = glm(score ~ shots + timeOnIce + primaryPosition, 
           data=data, family = binomial(link = "logit"))
AIC(glm1b) # 22894.29
BIC(glm1b) # 22944.67
# best model according to BIC


glm1c = glm(score ~ shots + timeOnIce + primaryPosition + weight, 
           data=data, family = binomial(link = "logit"))
AIC(glm1c) # 22891.6
BIC(glm1c) # 22950.38 ... BIC increasing again


glm1d = glm(score ~ shots + timeOnIce + primaryPosition + weight +
             nationality, 
           data=data, family = binomial(link = "logit"))
AIC(glm1d) # 22865.45


glm1e = glm(score ~ shots + timeOnIce + primaryPosition + weight +
             nationality + hits, 
           data=data, family = binomial(link = "logit"))
AIC(glm1e) # 22853.99


glm1f = glm(score ~ shots + timeOnIce + primaryPosition + weight + 
             nationality + hits + age, 
           data=data, family = binomial(link = "logit"))
AIC(glm1f) # 22829.79


glm1g = glm(score ~ shots + timeOnIce + primaryPosition + weight + 
             nationality + hits + age + faceoffTaken, 
           data=data, family = binomial(link = "logit"))
AIC(glm1g) # 22827.8
# best model according to AIC, we will use this as the final model


glm1h = glm(score ~ shots + timeOnIce + primaryPosition + weight + 
              nationality + hits + age + faceoffTaken + giveaways, 
            data=data, family = binomial(link = "logit"))
AIC(glm1h) # 22829.38 ... AIC increasing again

# clean up model name
glm1 = glm1g
summary(glm1)

### USING BACKWARDS REGRESSION
# fully saturated model
glm2 = glm(score ~ shots + timeOnIce + primaryPosition + weight + nationality +
             hits + age + faceoffTaken + giveaways + blocked + height_cm + takeaways +
             faceOffWins + assists + penaltyMinutes + shootsCatches + powerPlayAssists +
             shortHandedAssists,
           data = data, family = binomial(link = "logit"))

# use stepAIC backwards regression to find the best model
stepAIC(glm2, direction = 'backward', trace=FALSE)

# model specified by stepAIC
glm2 = glm(score ~ shots + timeOnIce + primaryPosition + weight + nationality +
           hits + age + height_cm + faceOffWins + powerPlayAssists,
           data=data, family = binomial(link="logit"))
summary(glm2)
AIC(glm2) # 22820.26
# Although this model has the lowest AIC, the stepAIC function may not lead to 
  # the best combination of predictors as it lends itself to overfitting and inconsistent results.

# Therefore, we select GLM1, as the final Bernoulli model
# Not only is this a more parsimonious model than GLM2, it was also built using a
  # random forest algorithm, guaranteed to not result in overfitting and provide
  # the most reliable results.

# Final model:
# score ~ shots + timeOnIce + primaryPosition + weight + nationality + hits + age + faceoffTaken

# ------ FINAL VISUALIZATIONS --------
# create visualizations using the most important variables

# colored scatter plot of top 2 variables - shots and timeOnIce
  # timeOnIce x-axis, shots y-axis, points colored by Position
ggplot(data = data) +
  geom_point(aes(x = timeOnIce, y = shots, color = primaryPosition)) +
  geom_jitter(aes(x = timeOnIce, y= shots, color = primaryPosition), alpha = I(0.7)) +
  labs(x = "Time On Ice", y = "Shots")+
  ggtitle("Shots by Position Player Time On Ice") +
  scale_color_manual(values=c("#744F28", "#FFB81C", "#DDCBa4","#FFFFFF"), "Position")

# histogram of score by age
ggplot(data = data) +
  geom_bar(aes(x = age, fill = score), position = "identity") +
  labs(x = "Age", y = "Frequency")+
  ggtitle("Goals scored by Age") +
  scale_fill_manual(values=c("#744F28", "#FFB81C"), "Goals \nScored")

# bar chart of primaryPosition and faceoffTaken
options(scipen = 999)
ggplot(data=data) +
  geom_col(aes(x=primaryPosition, y=faceoffTaken), fill="#744F28") +
  labs(x="Position", y="Number of Face Offs Taken") + 
  ggtitle("Total Face Offs Taken by Position") +
  scale_y_continuous(limits=c(0,100000), breaks=seq(0,100000,20000))


# histogram of nationality and goals scored
ggplot(data=data) +
  geom_bar(aes(x=nationality, fill=score), position="fill") +
  labs(x="Nationality", y="Proportion") +
  ggtitle("Proportion of Goals Scored by Nationality") +
  scale_fill_manual(values=c("#744F28", "#FFB81C"), "Goals \nScored")











