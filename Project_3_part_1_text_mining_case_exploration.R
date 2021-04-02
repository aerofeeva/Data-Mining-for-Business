#' Author: Anastasia Erofeeva
#' Date: November 27, 2020
#' Purpose: Case 3 Exploration
#' 


# Libraries
library(qdap)
library(tm)
library(ggplot2)
library(ggthemes)
library(dendextend)
library(wordcloud)
library(RColorBrewer)


# Working Directory
setwd("/cloud/project/Cases/III Gov Contractor")

# Options & Functions
options(stringsAsFactors = FALSE) #text strings will not be factors of categories
Sys.setlocale('LC_ALL','C') #some tweets are in different languages so you may get an error

# Lowercase function
tryTolower <- function(x){
  # return NA when there is an error
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error = function(e) e)
  # if not an error
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

# Cleaning function
cleanCorpus<-function(corpus){
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url)) 
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  return(corpus)
}

# Dendogram coloring function
colLab <- function(n) {
  if (is.leaf(n)) {
    a <- attributes(n)
    labCol <- labelColors[clusMember[which(names(clusMember) == a$label)]]
    attr(n, "nodePar") <- c(a$nodePar, lab.col = labCol)
  }
  n
}


### SAMPLE

train <- read.csv('student_tm_case_training_data.csv', header=TRUE)
test <- read.csv('student_tm_case_score_data.csv')
test$y <- NA # Add in the unknowns


### MODIFY

# Change column names to 'doc_id' and 'text'
names(train)[1] <- 'doc_id' 
names(train)[2] <- 'text'

# Change column names to 'doc_id' and 'text'
names(test)[1] <- 'doc_id' 
names(test)[2] <- 'text'
names(test)[3] <- 'label'

# Create custom stop words
customStopwords <- c(stopwords('english'), 'lol', 'smh', 'amp', 'rainbow','unicorn','zombie', 
                     'i...m', 'it...s', 'c', 'm', 'n', 's', 'also', 'don...t')

# Remove '<>'
train$text <- gsub('<.*?>','', train$text)

# Remove 'RT' (retweets)
train$text <- gsub('\\bRT\\b','', train$text)

# Remove URLs
train$text <- gsub('http.*','', train$text)

# Make a volatile corpus
originalCorpus <- VCorpus(DataframeSource(train))

# Preprocess the corpus
txtCorpus <- cleanCorpus(originalCorpus)

# Make a Document Term Matrix
txtDTM  <- DocumentTermMatrix(txtCorpus)
txtDTMm <- as.matrix(txtDTM)

# Make a Term Document Matrix
txtTDM  <- TermDocumentMatrix(txtCorpus)
txtTDMm <- as.matrix(txtTDM)


### EXPLORE

# Text of first 3 documents
head(train$text, 3)

# Number of documents labeled as related to political discourse
table(train$label)

# Plot y_click variable
ggplot(train, aes(x=as.factor(label), fill=as.factor(label))) + 
  geom_bar() +
  labs(title="Social Media Post Labels", x="Related to Political Discourse", y = "Count") +
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  scale_fill_manual(values=c("#b8b8b8", "#4c5bbd")) +
  theme_minimal() +
  theme(legend.position = "none")


# Frequency Data Frame
wordFreq <- colSums(txtDTMm)
wordFreq <- data.frame(word=names(wordFreq),frequency=wordFreq)

# Simple barplot with values greater than 50 
topWords      <- subset(wordFreq, wordFreq$frequency >= 50) 
topWords      <- topWords[order(topWords$frequency, decreasing=F),]

# Change to factor for ggplot
topWords$word <- factor(topWords$word, 
                        levels=unique(as.character(topWords$word))) 

ggplot(topWords, aes(x=word, y=frequency)) + 
  geom_bar(stat="identity", fill='#4c5bbd') + 
  coord_flip()+ theme_gdocs() +
  geom_text(aes(label=frequency), colour="white",hjust=1.25, size=5.0)


# Find word associations function
findAssociations<-function(word){
  # Inspect word associations for given word
  associations <- findAssocs(txtDTM, word, 0.15)
  associations
  
  # Organize the word associations
  wordDF <- data.frame(terms=names(associations[[1]]),
                        value=unlist(associations))
  wordDF$terms <- factor(wordDF$terms, levels=wordDF$terms)
  wordDF
  
  # Make a dot plot
  ggplot(wordDF, aes(y=terms)) +
    geom_point(aes(x=value), data=wordDF, col='#4c5bbd') +
    theme_gdocs() + 
    geom_text(aes(x=value,label=value), colour="#4c5bbd",hjust="inward", vjust ="inward" )
}

findAssociations('covid')
findAssociations('blacklivesmatter')
findAssociations('cnn')


# Reduce TDM
txtTDM2 <- removeSparseTerms(txtTDM, sparse=0.988) #shoot for ~50 terms; higher sparse = higher # of terms ie 97% of terms can have 0 and still be included.  98+% 0s not allowed or another way 3% of cells have a value  
txtTDM2

# Organize the smaller TDM
txtTDM2 <- as.data.frame(as.matrix(txtTDM2))

# Basic Hierarchical Clustering
hc <- hclust(dist(txtTDM2))
plot(hc,yaxt='n')

# Dendrogram
hcd         <- as.dendrogram(hc)
clusMember  <- cutree(hc, 4)
labelColors <- c("#616269", "#4c5bbd", "#425aff", "#0b1b80")
clusDendro <- dendrapply(hcd, colLab)

plot(clusDendro, 
     main = "Hierarchical Dendrogram", 
     type = "triangle",
     yaxt='n')
rect.dendrogram(hcd, k = 5, border = "grey50")


# Get row sums & organize
txtTDMv <- sort(rowSums(txtTDMm), decreasing = TRUE)
txtDF   <- data.frame(word = names(txtTDMv), freq = txtTDMv)

# Review all palettes
display.brewer.all()

# Choose a color & drop light ones
pal <- brewer.pal(8, "Blues")
pal <- pal[-(1:2)]

# Make simple word cloud
set.seed(1234)
wordcloud(txtDF$word,
          txtDF$freq,
          max.words=45,
          random.order=FALSE,
          colors=pal,
          scale=c(3,0.5))

# End
