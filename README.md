# STAT-172-Final-Project

Title: Boston Bruins Player Analysis

Authors: Ethan Marshall, Nick Pittman, Jack Schwartz

Description: The datasets analyzed came from Kaggle (datasource down below). By combining individual game data with a player information dataset, the goal of this analysis was to determine whether or not a hockey player will score in a game. Can we build a parsimonious model using modern statistical techniques to accurately predict this information?

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
We built two Random Forest models; one untuned and one tuned. After analyzing 22,950 Boston Bruins datapoints of invidiual performance, here are some statistics.

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

Forest 1 did a great job of accuractly predicting when a player will not score a goal, however it did a very poor job when a players actually scored a goal. Accuracy was relatively high, but given the large dataset and hockey's tendency to be very low scoring, it was extremely easy to predict that a player would not score. This caused our Accuracy metric to be partially biased. Forest 2 did a better job with accuracy, paticularly when players truly scored a goal. Specificity was significantly lower, but the model performed better!


Link to data: https://www.kaggle.com/datasets/martinellis/nhl-game-data

Link to RMarkdown: https://rpubs.com/jschwartz/STAT172_FINAL_BOSTON_BRUINS

