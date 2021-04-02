#' Author: Anastasia Erofeeva
#' Date: December 17, 2020
#' Purpose: Case 4
#' 


# Libs
library(dplyr)
library(vtreat)
library(caret)
library(DataExplorer)
library(ggplot2)
library(ggthemes)
library(lubridate)
library(rpart.plot)
library(randomForest)
library(MLmetrics)
library(pROC)

# Wd
setwd("/cloud/project/Cases/IV National City Bank/training")

# Raw data, need to add others
currentData   <- read.csv('CurrentCustomerMktgResults.csv')
vehicleData <- read.csv('householdVehicleData.csv') 
creditData <- read.csv('householdCreditData.csv')
axiomData <- read.csv('householdAxiomData.csv')

# Perform a join, need to add other data sets
joinData <- left_join(currentData, vehicleData, by = c('HHuniqueID'))
joinData <- left_join(joinData, creditData, by = c('HHuniqueID'))
joinData <- left_join(joinData, axiomData, by = c('HHuniqueID'))

# This is a classification problem so ensure R knows Y isn't 0/1 as integers
joinData$Y_AcceptedOffer <- as.factor(joinData$Y_AcceptedOffer)



## SAMPLE: Partition schema

set.seed(1234)
idx       <- createDataPartition(joinData$Y_AcceptedOffer,p=.7,list=F)
idx       <- idx[-1] # Remove one extra index
trainData <- joinData[idx,]
testData  <- joinData[-idx,]



## EXPLORE: EDA, perform your EDA

# Basic EDA
dim(trainData)
names(trainData)
summary(trainData)
sapply(trainData, class)
head(trainData)

# Dependent variable EDA
table(trainData$Y_AcceptedOffer)

# Plot Y_AcceptedOffer variable
ggplot(trainData, aes(x=Y_AcceptedOffer, fill=Y_AcceptedOffer)) + 
  geom_bar() +
  labs(title="Outcome Distribution", x="Outcome", y = "Count") +
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_fill_manual(values=c('#087054','#acb5b2'))

# Data Explorer
#create_report(trainData)

# Missing values
colSums(is.na(trainData))
plot_missing(trainData)

# Remove NAs from train data
clean_train <- na.omit(trainData)

# EDA for discrete variables
plotDiscreteVar<-function(varName, plotTitle, xLabel) {
  plotData <- clean_train %>%
    filter(Y_AcceptedOffer %in% c("Accepted", "DidNotAccept")) %>%
    group_by({{varName}}, Y_AcceptedOffer) %>%
    summarise(counts = n()) 
  
  # Stacked bar plots of y = counts by x = given variable,
  # colored by the variable Y_AcceptedOffer
  ggplot(plotData, aes(x = as.factor({{varName}}), y = counts)) +
    geom_bar(aes(fill = factor(Y_AcceptedOffer, levels = c("DidNotAccept", "Accepted"))),
             stat = "identity", position = position_stack()) +
    scale_color_discrete(guide=FALSE) +
    scale_fill_manual(values=c('#acb5b2', '#087054'), name="Outcome", 
                      breaks=c("DidNotAccept", "Accepted"), labels=c("Did Not Accept", "Accepted")) +
    labs(title=plotTitle, x=xLabel, y = "Count") +
    theme_minimal()
}

plotDiscreteVar(past_Outcome, "Outcome by Past Outcome", "Past Outcome")
plotDiscreteVar(Communication, "Outcome by Communication Type", "Communication Type")
plotDiscreteVar(LastContactMonth, "Outcome by Last Contact Month", "Last Contact Month")
plotDiscreteVar(LastContactDay, "Outcome by Last Contact Day", "Last Contact Day")
plotDiscreteVar(headOfhouseholdGender, "Outcome by Head of Household Gender", "Gender")
plotDiscreteVar(PetsPurchases, "Outcome by Purchases of Pet-Related Products", "Purchases of Pet-Related Products")
plotDiscreteVar(AffluencePurchases, "Outcome by Purchases of Luxury Items", "Purchases of Luxury Items")
plotDiscreteVar(Job, "Outcome by Head of Household Job", "Job")
plotDiscreteVar(Marital, "Outcome by Marital Status", "Marital Status")
plotDiscreteVar(Education, "Outcome by Education Level", "Education Level")
plotDiscreteVar(DefaultOnRecord, "Outcome by Default on Record", "Default on Record")
plotDiscreteVar(HHInsurance, "Outcome by Insurance", "Insurance")
plotDiscreteVar(CarLoan, "Outcome by Car Loan", "Car Loan")
plotDiscreteVar(DigitalHabits_5_AlwaysOn, "Outcome by Digital Habits", "Amount of time spent on web relative to other households")


# EDA for continuous variables
plotContinuousVar<-function(varName, plotTitle, yLabel) {
  # Medians
  var <- eval(substitute(varName), clean_train)
  medians <- aggregate(var ~  Y_AcceptedOffer, clean_train, median)
  
  # Box plot of y = given variable by x = Y_AcceptedOffer,
  # colored by the variable Y_AcceptedOffer
  ggplot(clean_train, aes(x = as.factor(Y_AcceptedOffer), y = var, fill = as.factor(Y_AcceptedOffer))) + 
    geom_boxplot() +
    geom_text(data = medians, aes(label = var, y = var), vjust = -0.9) +
    scale_color_discrete(guide=FALSE) +
    scale_fill_manual(values=c('#acb5b2', '#087054'), name="Outcome", 
                      breaks=c("DidNotAccept", "Accepted"), labels=c("Did Not Accept", "Accepted")) +
    labs(title=plotTitle, x = "Outcome", y = yLabel) +
    theme_minimal() + 
    theme(legend.position = "none")
}

plotContinuousVar(Age, "Outcome by Age", "Age")
plotContinuousVar(carYr, "Outcome by Car Year", "Car Year")
plotContinuousVar(NoOfContacts, "Outcome by Number of Current Contacts", "Number of Contacts During Current Campaign")
plotContinuousVar(DaysPassed, "Outcome by Days Passed", "Number of Days Since Last Contact for Previous Campaign")
plotContinuousVar(PrevAttempts, "Outcome by Number of Previous Contacts", "Number of Contacts During Previous Campaign")
plotContinuousVar(RecentBalance, "Outcome by Recent Balance", "Recent Balance")

# Calculate call duration
endtime   <- period_to_seconds(hms(clean_train$CallEnd))
starttime <- period_to_seconds(hms(clean_train$CallStart))
duration  <- round((endtime - starttime)/60, digits = 2) # Call duration in minutes

# Add callDuration column to dataset
clean_train$callDuration <- duration

# Plot outcome by call duration
plotContinuousVar(callDuration, "Outcome by Call Duration", "Call Duration in Minutes")

# Some variables were not plotted, because they had more than 50 categories.
# carMake: 65 categories
# carModel: 762 categories
# annualDonations: 286 categories
# EstRace: 94 categories

# Summary statistics for continuous variables
summary(clean_train$Age)
summary(clean_train$carYr)
summary(clean_train$NoOfContacts)
summary(clean_train$DaysPassed)
summary(clean_train$PrevAttempts)
summary(clean_train$RecentBalance)
summary(clean_train$callDuration)

# Remove callDuration column from dataset
clean_train$callDuration <- NULL



## MODIFY:

# Vtreat, need to declare xVars & name of Y var (remove variables with too many unique values:
# dataID, HHuniqueID, CallStart, CallEnd, carMake, carModel)
xVars <- c("Communication", "LastContactDay", "LastContactMonth", "NoOfContacts",
           "DaysPassed", "PrevAttempts", "past_Outcome", "carYr", "DefaultOnRecord", 
           "RecentBalance", "HHInsurance", "CarLoan", "headOfhouseholdGender", "annualDonations", 
           "EstRace", "PetsPurchases", "DigitalHabits_5_AlwaysOn", "AffluencePurchases",
           "Age", "Job", "Marital", "Education")
yVar  <- names(joinData)[12]
plan  <- designTreatmentsC(dframe        = joinData, #dataframe
                           varlist       = xVars, #input var vector
                           outcomename   = yVar, #y name
                           outcometarget = "Accepted") #success class)


# Apply the rules to the set
treatedTrain <- prepare(plan, trainData)
treatedTest  <- prepare(plan, testData)



## MODEL: caret etc.

# Model 1- Logistic regression
fitLog <- glm(Y_AcceptedOffer ~., data = treatedTrain, family = 'binomial', maxit=100)
summary(fitLog)

# Backward Variable selection to reduce chances of multi-colinearity
#bestFit <- step(fitLog, direction='backward')
#saveRDS(bestFit, 'bestFit.rds')
bestFit <- readRDS('bestFit.rds')
summary(bestFit)

# Refit logistic regression  
# Choose variables suggested by step function with p-values < 0.05
fitLog2 <- glm(Y_AcceptedOffer ~ Communication_catB + NoOfContacts + 
                 past_Outcome_catP + past_Outcome_catB + HHInsurance + 
                 EstRace_catB + PetsPurchases + Marital_catB + 
                 LastContactMonth_lev_x_apr + LastContactMonth_lev_x_aug + 
                 LastContactMonth_lev_x_feb + LastContactMonth_lev_x_jan + 
                 LastContactMonth_lev_x_jul + LastContactMonth_lev_x_jun + 
                 LastContactMonth_lev_x_may + LastContactMonth_lev_x_nov, 
               data = treatedTrain, family = 'binomial')
summary(fitLog2)

# Important variables
importance <- varImp(fitLog2, scale = FALSE)
barplot(importance$Overall, names.arg=rownames(importance))

# Plot top 10 important variables
ggplot(data=importance, aes(x = reorder(rownames(importance), Overall), y = Overall)) +
  geom_bar(stat="identity") +
  labs(title="Top 10 Important Variables",x="Variables", y = "Importance") +
  theme_minimal() + 
  coord_flip()


# Model 2 - KNN 
# knnFit <- train(as.factor(Y_AcceptedOffer) ~ ., #similar formula to lm
#                 data = treatedTrain, #data input
#                 method = "knn", #caret has other methods so specify KNN
#                 preProcess = c("center","scale"), #normalization
#                 tuneLength = 20, #tuning
#                 na.action = na.pass) #skip NAs
# saveRDS(knnFit, 'knn.rds')
knnFit <- readRDS('knn.rds')

# Choose optimal K value, K = 27
knnFit
plot(knnFit)


# Model 3- Random Forest

# Fit a random forest model with 5 trees
# rfFit1 = randomForest(Y_AcceptedOffer ~ ., 
#                              data=treatedTrain, 
#                              ntree=5, 
#                              mtry=2, 
#                              importance=TRUE)
# saveRDS(rfFit1,'rfFit1.rds')
rfFit1 <- readRDS('rfFit1.rds')
rfFit1

# Fit a random forest model with 100 trees
# rfFit2 = randomForest(Y_AcceptedOffer ~ ., 
#                       data=treatedTrain, 
#                       ntree=100, 
#                       mtry=2, 
#                       importance=TRUE)
# saveRDS(rfFit2,'rfFit2.rds')
rfFit2 <- readRDS('rfFit2.rds')
rfFit2

# Fit a random forest model with 500 trees
# rfFit3 = randomForest(Y_AcceptedOffer ~ ., 
#                       data=treatedTrain, 
#                       ntree=500, 
#                       mtry=2, 
#                       importance=TRUE)
# saveRDS(rfFit3,'rfFit3.rds')
rfFit3 <- readRDS('rfFit3.rds')
rfFit3

# Look at var importance
varImpPlot(rfFit2)



## ASSESS: Predict & calculate the KPI appropriate for classification

# Logistic Regression Training Set Evaluation
# Get train set predictions
logTrainPredictions <- predict(fitLog2, treatedTrain, type='response')

# Find best cutoff value
cutoffs <- seq(0, 1, by = 0.05) # define all cutoffs to check from 0.00 to 1, by .05 increments
accuracyLst <- list() # empty list to be filled in during the loop
for (i in seq(along = cutoffs)){
  prediction <- ifelse(logTrainPredictions >= cutoffs[i], "Accepted", "DidNotAccept") #Predicting for the specific cut-off
  accuracyResult <- Accuracy(trainData$Y_AcceptedOffer, prediction) # calculate the accuracy for the cutoff against actuals
  accuracyLst[[paste0('cutoff',cutoffs[i])]] <- accuracyResult # store the result in a list element
}
accuracies <- do.call(rbind, accuracyLst) # change the list to a matrix
accuracies <- data.frame(accuracies, row.names(accuracies)) # add the row names
accuracies[which.max(accuracies$accuracies),] # determine the maximum accuracy and corresponding cutoff value

# Classify 
cutoff      <- 1
logClasses <- ifelse(logTrainPredictions >= cutoff, "Accepted", "DidNotAccept")

# Organize w/Actual
logResults <- data.frame(actual  = trainData$Y_AcceptedOffer,
                         classes = logClasses,
                         probs   = logTrainPredictions)
head(logResults, 10)

# Get a confusion matrix
(confMat <- ConfusionMatrix(logResults$classes, logResults$actual))

# Logistic Regression Model Accuracy (training)
logAccuracy <- sum(diag(confMat)) / sum(confMat)
logAccuracy #40.11% (all predictions are "DidNotAccept")


# Logistic Regression Test Set Evaluation

# Get test set predictions
logTestPredictions <- predict(fitLog2, treatedTest, type='response')

# Find best cutoff value
cutoffs <- seq(0, 1, by = 0.05) # define all cutoffs to check from 0.00 to 1, by .05 increments
accuracyLst <- list() # empty list to be filled in during the loop
for (i in seq(along = cutoffs)){
  prediction <- ifelse(logTestPredictions >= cutoffs[i], "Accepted", "DidNotAccept") #Predicting for the specific cut-off
  accuracyResult <- Accuracy(testData$Y_AcceptedOffer, prediction) # calculate the accuracy for the cutoff against actuals
  accuracyLst[[paste0('cutoff',cutoffs[i])]] <- accuracyResult # store the result in a list element
}
accuracies <- do.call(rbind, accuracyLst) # change the list to a matrix
accuracies <- data.frame(accuracies, row.names(accuracies)) # add the row names
accuracies[which.max(accuracies$accuracies),] # determine the maximum accuracy and corresponding cutoff value

# Classify 
cutoff      <- 1
logTestClasses <- ifelse(logTestPredictions >= cutoff, "Accepted", "DidNotAccept")

# Organize w/Actual
logTestResults <- data.frame(actual  = testData$Y_AcceptedOffer,
                         classes = logTestClasses,
                         probs   = logTestPredictions)
head(logTestResults, 10)

# Get a confusion matrix
(confMat <- ConfusionMatrix(logTestResults$classes, logTestResults$actual))

# Logistic Regression Model Accuracy (test)
logTestAccuracy <- sum(diag(confMat)) / sum(confMat)
logTestAccuracy #40.08% (all predictions are "DidNotAccept")


#KNN Training Set Evaluation
knnClasses <- predict(knnFit,treatedTrain)
knnProbs <- predict(knnFit, treatedTrain, type=c('prob'))
knnResults <- data.frame(actual = treatedTrain$Y_AcceptedOffer, 
                         classes = knnClasses, 
                         probs = knnProbs)
head(knnResults, 10)

# Confusion Matrix
ConfusionMatrix(knnResults$classes, knnResults$actual)

# KNN Accuracy (training)
knnAccuracy <- Accuracy(knnResults$classes, knnResults$actual)
knnAccuracy # 75.07%


#KNN Test Set Evaluation
knnClassesTest <- predict(knnFit,treatedTest)
knnProbsTest <- predict(knnFit, treatedTest, type=c('prob'))
knnResultsTest <- data.frame(actual = treatedTest$Y_AcceptedOffer, 
                         classes = knnClassesTest, 
                         probs = knnProbsTest)
head(knnResultsTest, 10)

# Confusion Matrix
ConfusionMatrix(knnResultsTest$classes, knnResultsTest$actual)

# KNN Accuracy (test)
knnAccuracyTest <- Accuracy(knnResultsTest$classes, knnResultsTest$actual)
knnAccuracyTest # 72.42%


# Random Forest Training Set Evaluation

# Confusion Matrix for RF1
trainClass1 <- predict(rfFit1, treatedTrain)
confusionMatrix(trainClass1, as.factor(treatedTrain$Y_AcceptedOffer))

# Confusion Matrix for RF2
trainClass2 <- predict(rfFit2, treatedTrain)
confusionMatrix(trainClass2, as.factor(treatedTrain$Y_AcceptedOffer))

# Confusion Matrix for RF3
trainClass3 <- predict(rfFit3, treatedTrain)
confusionMatrix(trainClass3, as.factor(treatedTrain$Y_AcceptedOffer))

# Compare accuracy for 5, 100, and 500 trees
Accuracy(treatedTrain$Y_AcceptedOffer, predict(rfFit1,treatedTrain)) #67.14%
Accuracy(treatedTrain$Y_AcceptedOffer, predict(rfFit2,treatedTrain)) #81.21%
Accuracy(treatedTrain$Y_AcceptedOffer, predict(rfFit3,treatedTrain)) #80.93%


# Random Forest Test Set Evaluation

# Test confusion Matrix for RF1
testClass1 <- predict(rfFit1, treatedTest)
confusionMatrix(testClass1, as.factor(treatedTest$Y_AcceptedOffer))

# Test confusion Matrix for RF2
testClass2 <- predict(rfFit2, treatedTest)
confusionMatrix(testClass2, as.factor(treatedTest$Y_AcceptedOffer))

# test confusion Matrix for RF3
testClass3 <- predict(rfFit3, treatedTest)
confusionMatrix(testClass3, as.factor(treatedTest$Y_AcceptedOffer))

# Compare test accuracy for 5, 100, and 500 trees 
Accuracy(treatedTest$Y_AcceptedOffer, predict(rfFit1,treatedTest)) #66.58%
Accuracy(treatedTest$Y_AcceptedOffer, predict(rfFit2,treatedTest)) #71.75%
Accuracy(treatedTest$Y_AcceptedOffer, predict(rfFit3,treatedTest)) #71.50%


## NOW TO GET PROSPECTIVE CUSTOMER RESULTS

# 1. Load Raw Data
prospects <- read.csv('/cloud/project/Cases/IV National City Bank/ProspectiveCustomers.csv')

# 2. Join with external data
prospects <- left_join(prospects, vehicleData, by = c('HHuniqueID'))
prospects <- left_join(prospects, creditData, by = c('HHuniqueID'))
prospects <- left_join(prospects, axiomData, by = c('HHuniqueID'))

# 3. Apply a treatment plan
# Vtreat, need to declare xVars & name of Y var (remove variables with too many unique values:
# dataID, HHuniqueID, CallStart, CallEnd, carMake, carModel)
xVars <- c("Communication", "LastContactDay", "LastContactMonth", "NoOfContacts",
           "DaysPassed", "PrevAttempts", "past_Outcome", "carYr", "DefaultOnRecord", 
           "RecentBalance", "HHInsurance", "CarLoan", "headOfhouseholdGender", "annualDonations", 
           "EstRace", "PetsPurchases", "DigitalHabits_5_AlwaysOn", "AffluencePurchases",
           "Age", "Job", "Marital", "Education")
yVar  <- names(joinData)[12]
plan  <- designTreatmentsC(dframe        = joinData, #dataframe
                           varlist       = xVars, #input var vector
                           outcomename   = yVar, #y name
                           outcometarget = "Accepted") #success class)

# Apply the rules to the set of prospects
treatedProspects <- prepare(plan, prospects)

# 4. Make predictions using KNN model
prospectPreds <- predict(knnFit, treatedProspects, type= 'prob')

# 5. Join probabilities back to ID
prospectsResults <- cbind(prospects$HHuniqueID, prospectPreds)
prospectsResults

# 6. Identify the top 100 "success" class probabilities from prospectsResults
sum(prospectsResults$Accepted >= 0.5)
prospectsResultsDF <- data.frame(prospectsResults)
topProspects <- head(prospectsResultsDF[order(prospectsResultsDF$Accepted,decreasing = TRUE), ], 100)
topProspects$prospects.HHuniqueID
write.csv(topProspects$prospects.HHuniqueID,'top_100.csv', row.names = F)

# End
