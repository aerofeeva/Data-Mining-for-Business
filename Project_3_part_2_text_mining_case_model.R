#' Author: Anastasia Erofeeva
#' Date: November 27, 2020
#' Purpose: Case 3 Model Construction
#' 


# Libraries
library(text2vec)
library(caret)
library(tm)
library(glmnet)
library(lsa)
library(stringr)
library(MLmetrics)
library(pROC)

# Working Directory
setwd("/cloud/project/Cases/III Gov Contractor")

set.seed(1234)

# Create custom stop words
stops <- c(stopwords('english'), 'lol', 'smh', 'amp', 'rainbow','unicorn','zombie', 
           'i...m', 'it...s', 'c', 'm', 'n', 's', 'also', 'don...t')

textClean<-function(xVec, stops){
  xVec <- removePunctuation(xVec)
  xVec <- removeNumbers(xVec)
  xVec <- stripWhitespace(xVec)
  xVec <- tolower(xVec)
  xVec <- removeWords(xVec, stops)
  xVec <- str_trim(xVec, side = c("both", "left", "right"))
  xVec <- gsub('<.*?>','', xVec)
  xVec <- gsub('[^\x01-\x7F]', '', xVec)
  xVec <- gsub('\\brt\\b','', xVec)
  xVec <- gsub('http.*','', xVec)
  return(xVec)
}

# Get data
train <- read.csv('student_tm_case_training_data.csv', header=TRUE)
test <- read.csv('student_tm_case_score_data.csv')
test$y <- NA # Add in the unknowns

# Change train column names to 'doc_id' and 'text'
names(train)[1] <- 'doc_id' 
names(train)[2] <- 'text'

# Change test column names to 'doc_id' and 'text'
names(test)[1] <- 'doc_id' 
names(test)[2] <- 'text'
names(test)[3] <- 'label'



### ELASTIC NET ###

### SAMPLE : Partitioning
idx <- createDataPartition(train$label,p=.7,list=F)
trainTxt <- train[idx,]
validationTxt  <- train[-idx,]


### EXPLORE
# Please see 'erofeeva_TM_case_exploration.R' script
head(trainTxt$text,3)
table(trainTxt$label)


### MODIFY
trainTxt$text <- textClean(trainTxt$text, stops)
head(trainTxt$text,10)

# Initial iterator to make vocabulary
iterMaker <- itoken(trainTxt$text, 
                    preprocess_function = list(tolower), 
                    progressbar         = T)
textVocab <- create_vocabulary(iterMaker, stopwords=c(stopwords('english'), 'lol', 'smh', 'amp', 'rainbow','unicorn','zombie', 
                                                      'i...m', 'it...s', 'c', 'also', 'don...t'))
head(textVocab)
tail(textVocab, 10) # Top 10 most frequent words
nrow(textVocab)

#prune vocab to make DTM smaller
prunedtextVocab <- prune_vocabulary(textVocab,
                                    term_count_min = 10, # Only include words that appear 10 or more times
                                    doc_proportion_max = 0.5,
                                    doc_proportion_min = 0.001)
nrow(prunedtextVocab)

# Using the pruned vocabulary to declare the DTM vectors 
vectorizer <- vocab_vectorizer(prunedtextVocab)

# Take the vocabulary lexicon and the pruned text function to make a DTM 
tweetsDTM <- create_dtm(iterMaker, vectorizer)
dim(tweetsDTM)


### MODEL
#train text only model
textFit <- cv.glmnet(tweetsDTM,
                     y=as.factor(trainTxt$label),
                     alpha=0.9,
                     family='binomial',
                     type.measure='auc',
                     nfolds=5,
                     intercept=F)


# Examine
head(coefficients(textFit),10)
tail(coefficients(textFit),10)


# Subset to impacting terms
bestTerms <- subset(as.matrix(coefficients(textFit)), 
                    as.matrix(coefficients(textFit)) !=0)
bestTerms <- data.frame(tokens = rownames(bestTerms),
                        coeff  = bestTerms)
rownames(bestTerms) <- NULL
head(bestTerms[order(bestTerms$X1, decreasing = T),], 10)
nrow(bestTerms)
ncol(tweetsDTM)

# Make training predictions
trainingPreds <- predict(textFit, tweetsDTM, type = 'class')
confusionMatrix(as.factor(trainingPreds),
                as.factor(trainTxt$label))

# Find best cutoff value
cutoffs <- seq(0, 1, by = 0.05) # define all cutoffs to check from 0.00 to 1, by .05 increments
accuracyLst <- list() # empty list to be filled in during the loop
for (i in seq(along = cutoffs)){
  prediction <- ifelse(trainingPreds >= cutoffs[i], 1, 0) #Predicting for the specific cut-off
  accuracyResult <- Accuracy(trainTxt$label, prediction) # calculate the accuracy for the cutoff against actuals
  accuracyLst[[paste0('cutoff',cutoffs[i])]] <- accuracyResult # store the result in a list element
}
accuracies <- do.call(rbind, accuracyLst) # change the list to a matrix
accuracies <- data.frame(accuracies, row.names(accuracies)) # add the row names
accuracies[which.max(accuracies$accuracies),] # determine the maximum accuracy and corresponding cutoff value

# Classify 
cutoff      <- 0.05
enClasses <- ifelse(trainingPreds >= cutoff, 1,0)

# Organize w/Actual
enResults <- data.frame(actual = trainTxt$label,
                        classes = as.factor(enClasses),
                        probs = as.factor(trainingPreds))
head(enResults, 10)



### ASSESS

### Apply to new tweets requires the construction of the new tweet DTM exactly as the training set
validationIT <- itoken(validationTxt$text, 
                   tokenizer = word_tokenizer)

# Use the same vectorizer but with new iterator
validationDTM <- create_dtm(validationIT,vectorizer)

validationPreds <- predict(textFit, validationDTM, type = 'class')
confusionMatrix(as.factor(validationPreds),
                as.factor(validationTxt$label))



### LATENT SEMANTIC ANALYSIS ###

# Combine into a single corpus; unify encoding just in case
allTxt        <- rbind(train, test)
allTxt$status <- c(rep('training',2000),rep('test',nrow(test)))
allTxt$text   <- enc2utf8(allTxt$text)

# Clean the text
allTxt$text <- textClean(allTxt$text, stops)

# Build the Modeling Matrix
# Make vocabulary
iterMaker <- itoken(allTxt$text, 
                    progressbar = T)
textVocab <- create_vocabulary(iterMaker)

#prune vocab to make DTM smaller
prunedtextVocab <- prune_vocabulary(textVocab,
                                    term_count_min = 10,
                                    doc_proportion_max = 0.5,
                                    doc_proportion_min = 0.001)

# Using the pruned vocabulary to declare the DTM vectors 
vectorizer <- vocab_vectorizer(prunedtextVocab)

# Finally, make a DTM 
txtDTM <- create_dtm(iterMaker, vectorizer)
dim(txtDTM)

# LSA Needs a TDM, it works on columns, so you have to transpose it
lsaTDM <- lsa(t(as.matrix(txtDTM)), 10)
saveRDS(lsaTDM, 'lsaTDM.rds')
lsaTDM <- readRDS('lsaTDM.rds')

# Get the modeling part out
modelingVectors <- as.data.frame(lsaTDM$dk)
head(modelingVectors)

# Append the meta back
modelingMatrix <- data.frame(doc_id = allTxt$doc_id, 
                             modelingVectors, 
                             label  = allTxt$label, 
                             status = allTxt$status)

# To save iteration time you can save out copies w/write.csv for each section
lsaTrain <- subset(modelingMatrix, modelingMatrix$status=='training')
lsaTest <- subset(modelingMatrix, modelingMatrix$status=='test')


### SAMPLE : Partitioning
index <- sample(1:2000,.7*2000) 
trainTxt <- lsaTrain[index,]
validationTxt <- lsaTrain[-index,]


### MODIFY
trainTxt$doc_id <- NULL # drop unneeded columns
trainTxt$status <- NULL


### MODEL (Logistic Regression)

logFit <- glm(label~., trainTxt, family = 'binomial')
summary(logFit)

# Refit logistic regression keeping only variables with p-values < 0.05
logFit2 <- glm(label ~ V1 + V2 + V4 + V5 + V6 + V7 + V8 + V10, 
               trainTxt, family = 'binomial')
summary(logFit2)

# Logistic Regression Training Set Evaluation
trainingPreds <- predict(logFit2, trainTxt, type = c('resp'))
trainingPreds <-ifelse(trainingPreds>=.5,1,0)
table(trainingPreds, trainTxt$label)

# Find best cutoff value
cutoffs <- seq(0, 1, by = 0.05) # define all cutoffs to check from 0.00 to 1, by .05 increments
accuracyLst <- list() # empty list to be filled in during the loop
for (i in seq(along = cutoffs)){
  prediction <- ifelse(trainingPreds >= cutoffs[i], 1, 0) #Predicting for the specific cut-off
  accuracyResult <- Accuracy(trainTxt$label, prediction) # calculate the accuracy for the cutoff against actuals
  accuracyLst[[paste0('cutoff',cutoffs[i])]] <- accuracyResult # store the result in a list element
}
accuracies <- do.call(rbind, accuracyLst) # change the list to a matrix
accuracies <- data.frame(accuracies, row.names(accuracies)) # add the row names
accuracies[which.max(accuracies$accuracies),] # determine the maximum accuracy and corresponding cutoff value

# Classify 
cutoff <- 0.05
txtClasses <- ifelse(trainingPreds >= cutoff, 1,0)

# Organize w/Actual
logResults <- data.frame(actual  = trainTxt$label,
                         classes = txtClasses,
                         probs   = trainingPreds)
head(logResults, 10)

# Get a confusion matrix
(confMat <- ConfusionMatrix(logResults$classes, logResults$actual))

# Logistic Regression Model Accuracy (training)
logAccuracy <- sum(diag(confMat)) / sum(confMat)
logAccuracy

# Visually how well did we separate our classes?
ggplot(logResults, aes(x=trainingPreds, color=as.factor(actual))) +
  geom_density() + 
  geom_vline(aes(xintercept = cutoff), color = 'green')

# ROC
ROCobj <- roc(logResults$classes, logResults$actual)
plot(ROCobj)

# AUC
AUC(logResults$actual, logResults$classes)


### ASSESS (Logistic Regression)

# Logistic regression validation set evaluation
validationTxt$doc_id <- NULL
validationTxt$status <- NULL

validationPreds <- predict(logFit2, validationTxt, type = c('resp'))
validationPreds <-ifelse(validationPreds>=.5,1,0)
table(validationPreds, validationTxt$label)

# Find best cutoff value
cutoffs <- seq(0, 1, by = 0.05) # define all cutoffs to check from 0.00 to 1, by .05 increments
accuracyLst <- list() # empty list to be filled in during the loop
for (i in seq(along = cutoffs)){
  prediction <- ifelse(validationPreds >= cutoffs[i], 1, 0) #Predicting for the specific cut-off
  accuracyResult <- Accuracy(validationTxt$label, prediction) # calculate the accuracy for the cutoff against actuals
  accuracyLst[[paste0('cutoff',cutoffs[i])]] <- accuracyResult # store the result in a list element
}
accuracies <- do.call(rbind, accuracyLst) # change the list to a matrix
accuracies <- data.frame(accuracies, row.names(accuracies)) # add the row names
accuracies[which.max(accuracies$accuracies),] # determine the maximum accuracy and corresponding cutoff value

# Classify 
cutoff <- 0.05
validationClasses <- ifelse(validationPreds >= cutoff, 1,0)

# Organize w/Actual
logResults <- data.frame(actual  = validationTxt$label,
                         classes = validationClasses,
                         probs   = validationPreds)
head(logResults, 10)

# Get a confusion matrix
(confMat <- ConfusionMatrix(logResults$classes, logResults$actual))

# Logistic Regression Model Accuracy (validation)
logValidationAccuracy <- sum(diag(confMat)) / sum(confMat)
logValidationAccuracy

# Once you have a model you are ok with, you can score new data but CANNOT test accuracy because you don't know actuals!  This is just like real life, eventually you could know but not at the time you make predictions.
# Logistic regression model generalizes to new data better, so we will use it to make classifications
yHat <- predict(logFit2, lsaTest)
yHat <-ifelse(yHat>=.5,1,0)
yHat
table(yHat) 
write.csv(yHat,'erofeeva_TM_scores.csv', row.names = F) #save a copy for your case

# End