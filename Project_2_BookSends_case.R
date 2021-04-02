#' Author: Anastasia Erofeeva
#' Date: October 23, 2020
#' Purpose: Case 2
#' 


# Libraries
library(vtreat)
library(caret)
library(DataExplorer)
library(MLmetrics)
library(pROC)
library(ggplot2)
library(ggthemes)
library(dplyr)

# Options
options(stringsAsFactors = F)

# Working Directory
setwd("/cloud/project/Cases/II Ad Tech")


# SAMPLE
adData <- read.csv('Case2_final_adTech_data.csv')

# Train 90%/Test 10% Partitioning
splitPercent <- round(nrow(adData) %*% .9)
totalRecords <- 1:nrow(adData)
set.seed(1234)
idx <- sample(totalRecords, splitPercent)
train <- adData[idx,]
test  <- adData[-idx,]


# EXPLORE
table(train$y_click)

# Plot y_click variable
ggplot(train, aes(x=as.factor(y_click), fill=as.factor(y_click))) + 
  geom_bar() +
  labs(title="Click Through Distribution", x="Clicked", y = "Count") +
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  theme_minimal() +
  theme(legend.position = "none")

# CTR
clicks <- table(train$y_click)
ctr <- clicks["1"] / (clicks["0"] + clicks["1"])
ctr

# Basic EDA
dim(train)
names(train)
summary(train)
sapply(train, class)
head(train)

# Data Explorer
#create_report(train)
plot_histogram(train$Total_Past_Communications)
plot_bar(train$Browser)

# Missing values
colSums(is.na(train))
plot_missing(train)

# Remove NAs from train data
clean_train <- na.omit(train)

# EDA by variable

# Clicks by Browser
clicks_browser <- clean_train %>%
  filter(y_click %in% c(0, 1)) %>%
  group_by(Browser, y_click) %>%
  summarise(counts = n()) 

# Stacked bar plots of y = counts by x = Browser,
# colored by the variable y_click
ggplot(clicks_browser, aes(x = reorder(Browser, -counts), y = counts)) +
  geom_bar(aes(color = as.factor(y_click), fill = as.factor(y_click)),
           stat = "identity", position = position_stack()) +
  scale_color_discrete(guide=FALSE) +
  scale_fill_discrete(name="Clicked", breaks=c(0, 1), labels=c("No", "Yes")) +
  labs(title="Clicks by Browser",x="Browser", y = "Counts") +
  theme_minimal()

# Clicks by Genre
clicks_genre <- clean_train %>%
  filter(y_click %in% c(0, 1)) %>%
  group_by(Genre, y_click) %>%
  summarise(counts = n()) 

# Stacked bar plots of y = counts by x = Genre,
# colored by the variable y_click
ggplot(clicks_genre, aes(x = reorder(Genre, -counts), y = counts)) +
  geom_bar(aes(color = as.factor(y_click), fill = as.factor(y_click)),
           stat = "identity", position = position_stack()) +
  scale_color_discrete(guide=FALSE) +
  scale_fill_discrete(name="Clicked", breaks=c(0, 1), labels=c("No", "Yes")) +
  labs(title="Clicks by Genre",x="Genre", y = "Counts") +
  theme_minimal()

# Clicks by Sent Hour
clicks_hour <- clean_train %>%
  filter(y_click %in% c(0, 1)) %>%
  group_by(Sent_Hour, y_click) %>%
  summarise(counts = n()) 

# Stacked bar plots of y = counts by x = Sent_Hour,
# colored by the variable y_click
ggplot(clicks_hour, aes(x = reorder(Sent_Hour, -counts), y = counts)) +
  geom_bar(aes(color = as.factor(y_click), fill = as.factor(y_click)),
           stat = "identity", position = position_stack()) +
  scale_color_discrete(guide=FALSE) +
  scale_fill_discrete(name="Clicked", breaks=c(0, 1), labels=c("No", "Yes")) +
  labs(title="Clicks by Sent Hour",x="Sent Hour", y = "Counts") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Clicks by Sign Up Site
clicks_site <- clean_train %>%
  filter(y_click %in% c(0, 1)) %>%
  group_by(Signup_Site, y_click) %>%
  summarise(counts = n()) 

# Stacked bar plots of y = counts by x = Signup_Site,
# colored by the variable y_click
ggplot(clicks_site, aes(x = reorder(Signup_Site, -counts), y = counts)) +
  geom_bar(aes(color = as.factor(y_click), fill = as.factor(y_click)),
           stat = "identity", position = position_stack()) +
  scale_color_discrete(guide=FALSE) +
  scale_fill_discrete(name="Clicked", breaks=c(0, 1), labels=c("No", "Yes")) +
  labs(title="Clicks by Sign Up Site",x="Sign Up Site", y = "Counts") +
  theme_minimal()

# Clicks by Customer Location
clicks_location <- clean_train %>%
  filter(y_click %in% c(0, 1)) %>%
  group_by(Customer_Location, y_click) %>%
  summarise(counts = n()) 

# Stacked bar plots of y = counts by x = Customer_Location,
# colored by the variable y_click
ggplot(clicks_location, aes(x = Customer_Location, y = counts)) +
  geom_bar(aes(color = as.factor(y_click), fill = as.factor(y_click)),
           stat = "identity", position = position_stack()) +
  scale_color_discrete(guide=FALSE) +
  scale_fill_discrete(name="Clicked", breaks=c(0, 1), labels=c("No", "Yes")) +
  labs(title="Clicks by Customer Location",x="State", y = "Counts") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Word_Count summary statistics
summary(clean_train$Word_Count)
word_medians <- aggregate(Word_Count ~  y_click, clean_train, median)

# Box plot of y = Word_Count by x = y_click,
# colored by the variable y_click
ggplot(clean_train, aes(x = as.factor(y_click), y = Word_Count, fill = as.factor(y_click))) + 
  geom_boxplot() +
  geom_text(data = word_medians, aes(label = Word_Count, y = Word_Count), vjust = -0.9) +
  scale_color_discrete(guide=FALSE) +
  scale_fill_discrete(name="Clicked", breaks=c(0, 1), labels=c("No", "Yes")) +
  labs(title="Clicks by Word Count", x = "Clicked", y = "Word Count") +
  theme_minimal() + 
  theme(legend.position = "none")

# Subject_Marketability_score summary statistics
summary(clean_train$Subject_Marketability_score)
subject_medians <- aggregate(Subject_Marketability_score ~  y_click, clean_train, median)

# Box plot of y = Subject_Marketability_score by x = y_click,
# colored by the variable y_click
ggplot(clean_train, aes(x = as.factor(y_click), y = Subject_Marketability_score, fill = as.factor(y_click))) + 
  geom_boxplot() +
  geom_text(data = subject_medians, aes(label = Subject_Marketability_score, y = Subject_Marketability_score), vjust = -0.9) +
  scale_color_discrete(guide=FALSE) +
  scale_fill_discrete(name="Clicked", breaks=c(0, 1), labels=c("No", "Yes")) +
  labs(title="Clicks by Subject Marketability Score", x = "Clicked", y = "Subject Marketability Score") +
  theme_minimal() + 
  theme(legend.position = "none")

# Total_Past_Communications summary statistics
summary(clean_train$Total_Past_Communications)
past_medians <- aggregate(Total_Past_Communications ~ y_click, clean_train, median)

# Box plot of y = Total_Past_Communications by x = y_click,
# colored by the variable y_click
ggplot(clean_train, aes(x = as.factor(y_click), y = Total_Past_Communications, fill = as.factor(y_click))) + 
  geom_boxplot() +
  geom_text(data = past_medians, aes(label = Total_Past_Communications, y = Total_Past_Communications), vjust = -1) +
  scale_color_discrete(guide=FALSE) +
  scale_fill_discrete(name="Clicked", breaks=c(0, 1), labels=c("No", "Yes")) +
  labs(title="Clicks by Total Past Communications", x = "Clicked", y = "Total Past Communications") +
  theme_minimal() + 
  theme(legend.position = "none")

# Total_Links summary statistics
summary(clean_train$Total_Links)
links_medians <- aggregate(Total_Links ~ y_click, clean_train, median)

# Box plot of y = Total_Links by x = y_click,
# colored by the variable y_click
ggplot(clean_train, aes(x = as.factor(y_click), y = Total_Links, fill = as.factor(y_click))) + 
  geom_boxplot() +
  geom_text(data = links_medians, aes(label = Total_Links, y = Total_Links), vjust = -2) +
  scale_color_discrete(guide=FALSE) +
  scale_fill_discrete(name="Clicked", breaks=c(0, 1), labels=c("No", "Yes")) +
  labs(title="Clicks by Total Links", x = "Clicked", y = "Total Links") +
  theme_minimal() + 
  theme(legend.position = "none")

# Total_Images summary statistics
summary(clean_train$Total_Images)
images_medians <- aggregate(Total_Images ~ y_click, clean_train, median)

# Box plot of y = Total_Images by x = y_click,
# colored by the variable y_click
ggplot(clean_train, aes(x = as.factor(y_click), y = Total_Images, fill = as.factor(y_click))) + 
  geom_boxplot() +
  geom_text(data = images_medians, aes(label = Total_Images, y = Total_Images), vjust = -2) +
  scale_color_discrete(guide=FALSE) +
  scale_fill_discrete(name="Clicked", breaks=c(0, 1), labels=c("No", "Yes")) +
  labs(title="Clicks by Total Images", x = "Clicked", y = "Total Images") +
  theme_minimal() +
  theme(legend.position = "none")


# MODIFY 

# Prepare data for modeling
xVars <- names(train)[2:11] #names of the input features
y     <- names(train)[12] #name of the y variable

# Variable treatment
plan <- designTreatmentsC(dframe        = train, #dataframe
                          varlist       = xVars, #input var vector
                          outcomename   = y, #y name
                          outcometarget = 1) #success class

# Prepare data using plan
treatedTrain <- prepare(plan, train)
treatedTest  <- prepare(plan, test)

# Get 20% train subset for fitting purposes
splitPercent2  <- round(nrow(train) %*% .2)
(totalRecords <- 1:nrow(train))
set.seed(1234)
idx <- sample(totalRecords, splitPercent2)
train2 <- train[idx, ]
treatedTrain2 <- prepare(plan, train2)

# Dimensions
dim(treatedTrain2)

# Get 10% train subset for KNN
splitPercent3  <- round(nrow(train) %*% .1)
(totalRecords <- 1:nrow(train))
set.seed(1234)
idx <- sample(totalRecords, splitPercent3)
train3 <- train[idx, ]
treatedTrain3 <- prepare(plan, train3)

# Remove isBad columns for KNN
treatedTrain3[ ,c('Subject_Marketability_score_isBAD', 
                  'Total_Past_Communications_isBAD', 'Word_Count_isBAD',
                  'Total_Links_isBAD', 'Total_Images_isBAD')] <- list(NULL)

# Dimensions
dim(treatedTrain3)


# MODEL

# Model - Logistic regression using 20% of train data
fitLog <- glm(y_click ~., data = treatedTrain2, family = 'binomial')
summary(fitLog)

# Backward Variable selection to reduce chances of multi-colinearity
#bestFit <- step(fitLog, direction='backward')
#saveRDS(bestFit, 'bestFit.rds')
bestFit <- readRDS('bestFit.rds')
summary(bestFit)

# Compare model size
length(coefficients(fitLog))
length(coefficients(bestFit))

# Refit model using full train set 
# Choose variables suggested by step function with p-values < 0.05
fitLog2 <- glm(y_click ~ Subject_Marketability_score + Customer_Location_catB + 
                 Total_Past_Communications + Total_Past_Communications_isBAD + 
                 Word_Count + Total_Images + Genre_catB + Sent_Hour_lev_x_03_colon_30_colon_00 + 
                 Sent_Hour_lev_x_10_colon_00_colon_00 + Sent_Hour_lev_x_11_colon_30_colon_00, 
               data = treatedTrain, family = 'binomial')
summary(fitLog2)

# Repeat Backward Variable selection using full train set and chosen variables
bestFit2 <- step(fitLog2, direction='backward')
summary(bestFit2)
plot(bestFit2)

# Important variables
importance <- varImp(bestFit2, scale = FALSE)
barplot(importance$Overall, names.arg=rownames(importance))

# Plot top 10 important variables
ggplot(data=importance, aes(x = reorder(rownames(importance), Overall), y = Overall)) +
  geom_bar(stat="identity") +
  labs(title="Top 10 Important Variables",x="Variables", y = "Importance") +
  theme_minimal() + 
  coord_flip()

# Model - KNN using 10% of train data
# knnFit <- train(as.factor(y_click) ~ ., #similar formula to lm
#                 data = treatedTrain3, #data input
#                 method = "knn", #caret has other methods so specify KNN
#                 preProcess = c("center","scale"), #normalization
#                 tuneLength = 20, #tuning
#                 na.action = na.pass) #skip NAs
# saveRDS(knnFit, 'knn.rds')
knnFit <- readRDS('knn.rds')

# Choose optimal K value, K = 23
knnFit
plot(knnFit)


# ASSESS

# Logistic Regression Training Set Evaluation

# Get predictions
adPredictions <- predict(bestFit2,  treatedTrain, type='response')

# Find best cutoff value
cutoffs <- seq(0, 1, by = 0.05) # define all cutoffs to check from 0.00 to 1, by .05 increments
accuracyLst <- list() # empty list to be filled in during the loop
for (i in seq(along = cutoffs)){
  prediction <- ifelse(adPredictions >= cutoffs[i], 1, 0) #Predicting for the specific cut-off
  accuracyResult <- Accuracy(train$y_click, prediction) # calculate the accuracy for the cutoff against actuals
  accuracyLst[[paste0('cutoff',cutoffs[i])]] <- accuracyResult # store the result in a list element
}
accuracies <- do.call(rbind, accuracyLst) # change the list to a matrix
accuracies <- data.frame(accuracies, row.names(accuracies)) # add the row names
accuracies[which.max(accuracies$accuracies),] # determine the maximum accuracy and corresponding cutoff value

# Classify 
cutoff      <- 0.7
adClasses <- ifelse(adPredictions >= cutoff, 1,0)

# Organize w/Actual
logResults <- data.frame(actual  = train$y_click,
                      classes = adClasses,
                      probs   = adPredictions)
head(logResults, 10)

# Get a confusion matrix
(confMat <- ConfusionMatrix(logResults$classes, logResults$actual))

# Logistic Regression Model Accuracy (training)
logAccuracy <- sum(diag(confMat)) / sum(confMat)

# Visually how well did we separate our classes?
ggplot(logResults, aes(x=adPredictions, color=as.factor(actual))) +
  geom_density() + 
  geom_vline(aes(xintercept = cutoff), color = 'green')

# ROC
ROCobj <- roc(logResults$classes, logResults$actual)
plot(ROCobj)

# AUC
AUC(logResults$actual, logResults$classes)

#KNN Training Set Evaluation
knnClasses <- predict(knnFit,treatedTrain)
knnProbs <- predict(knnFit, treatedTrain, type=c('prob'))
knnResults <- data.frame(actual = treatedTrain$y_click, 
                         classes = knnClasses, 
                         probs = knnProbs)
head(knnResults, 10)

# Confusion Matrix
table(knnResults$classes, knnResults$actual)

# KNN Accuracy (training)
knnAccuracy <- Accuracy(knnResults$classes, knnResults$actual)

# Compare accuracy of log and knn models
logAccuracy
knnAccuracy
max(logAccuracy, knnAccuracy)

# Log model has slightly higher accuracy, so we will use it to make classifications

# Get predictions for test set
testPredictions <- predict(bestFit2,  treatedTest, type='response')

# Find best cutoff value for test set
cutoffs <- seq(0, 1, by = 0.05) # define all cutoffs to check from 0.00 to 1, by .05 increments
accuracyLst <- list() # empty list to be filled in during the loop
for (i in seq(along = cutoffs)){
  prediction <- ifelse(adPredictions >= cutoffs[i], 1, 0) #Predicting for the specific cut-off
  accuracyResult <- Accuracy(test$y_click, prediction) # calculate the accuracy for the cutoff against actuals
  accuracyLst[[paste0('cutoff',cutoffs[i])]] <- accuracyResult # store the result in a list element
}
accuracies <- do.call(rbind, accuracyLst) # change the list to a matrix
accuracies <- data.frame(accuracies, row.names(accuracies)) # add the row names
accuracies[which.max(accuracies$accuracies),] # determine the maximum accuracy and corresponding cutoff value

# Classify for test set
cutoff      <- 0.9
testClasses <- ifelse(testPredictions >= cutoff, 1,0)

# Organize w/Actual
logResults <- data.frame(actual  = test$y_click,
                         classes = testClasses,
                         probs   = testPredictions)
head(logResults)

# Get a confusion matrix for test set
(confMat <- ConfusionMatrix(logResults$classes, logResults$actual))

# Logistic Regression Model Accuracy (test)
logTestAccuracy <- sum(diag(confMat)) / sum(confMat)
logTestAccuracy

# Compare train and test set accuracy
logAccuracy
logTestAccuracy

# End

