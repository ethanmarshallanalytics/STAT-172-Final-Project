# STAT-172-Final-Project

Title: Boston Bruins Player Analysis

Authors: Ethan Marshall, Nick Pittman, Jack Schwartz

Description: The datasets we analyzed came from Kaggle (datasource down below). We took individual player game data and combined this dataset with a player information dataset. The goal of the analysis is to determine whether or not a hockey player scores in a game. Can we build a model that can accurately predict if a player can score in a particular game?

Programming Language: R-Studio

Packages Used:
dyplr
tidyr
ggplot2
lubridate
devtools
paletti
patchwork
RColorBrewer
ramdomForest
pROC
tidytext
reshape2
glmnet
MASS

Steps in the Data:
1) Import Data and Clean the dataset
2) Add Explanatory plots to have a better visual understanding of the data
3) Run Forest 1
4) Tune Forest and run Forest 2
5) Analyze rocCurve
6) Build Bernoulli Distribution Model
7) Build Visualizations to Analyze Results

To examine the code and work please see the file named: Boston Bruins Player Analysis.Rmd or click the link to the RMarkdown.

Random Forest Outcome:
We built two Random Forest models. Our first model was untuned and our second model was tuned. After analyzing 22,950 Boston Bruins players games attached below are some stats.

Forest 1 (Untuned):
Accuracy: 86.9%
Sensitiity: 50.6%
Specificity: 88.3%
False Positive %: 11.7%

Forest 2 (Tuned):
Accuracy: 87.1%
Sensitiity: 76.1%
Specificity: 70.4%
False Positive %: 29.6%  

Forest 1 does a great job at accuractly predicting a player will not score a goal, however it does a very poor job of accuratley predicting a player scoring a goal. Accuracy is relatively high, but with a large dataset and scoring a goal in hockey being difficult, it is extremely easy to predict a player not scoring. This causes our Accuracy metric to be partially biased. Forest 2 does a better job with accuracy, and actually predicting a player scoring a goal. Specificity is significantly lower, but the model is performing better!


Link to data: https://www.kaggle.com/datasets/martinellis/nhl-game-data

Link to RMarkdown: https://rpubs.com/jschwartz/STAT172_FINAL_BOSTON_BRUINS

