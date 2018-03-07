# Unit 5 - Twitter


# VIDEO 5

# Read in the data
setwd("E:\\Data Analytics with RET\\Text Scrapping")
tweets = read.csv("tweets.csv", stringsAsFactors=FALSE)
head(tweets)
str(tweets)
View(tweets)

# Create dependent variable

tweets$Negative = as.factor(tweets$Avg <= -1)

table(tweets$Negative)

tweets$Negative = x$Neg

# Install new packages

install.packages("tm")
library(tm)

install.packages("SnowballC")
library(SnowballC)

# Create corpus: Corpus is collection of documents
corpus = VCorpus(VectorSource(tweets$Tweet)) 
# A vector source interprets each element of the vector x as a document.

# Look at corpus


corpus[[1]]$content

# Convert to lower-case

corpus = tm_map(corpus, content_transformer(tolower))

corpus[[1]]$content

# Remove punctuation

corpus = tm_map(corpus, removePunctuation)

corpus[[1]]$content

# Look at stop words 

stopwords("english")[1:10]

# Remove stopwords and apple

corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))

corpus[[1]]$content

# Stem document 

corpus = tm_map(corpus, stemDocument)

corpus[[1]]$content




# Video 6

# Create matrix

frequencies = DocumentTermMatrix(corpus)

frequencies

# Look at matrix 

inspect(frequencies[1000:1005,505:515])

# Check for sparsity

findFreqTerms(frequencies, lowfreq=20)

# Remove sparse terms

sparse = removeSparseTerms(frequencies, 0.995)
sparse

# Convert to a data frame

class(sparse)

tweetsSparse = as.data.frame(as.matrix(sparse))

class(tweetsSparse)

# Make all variable names R-friendly

colnames(tweetsSparse) = make.names(colnames(tweetsSparse))

colnames(tweetsSparse)[1:10]

# Add dependent variable
tweetsSparse$Negative = tweets$Negative
head(tweetsSparse)[1:7]

# Split the data
library(caTools)
set.seed(123)

split = sample.split(tweetsSparse$Negative, SplitRatio = 0.7)

trainSparse = subset(tweetsSparse, split==TRUE)
testSparse = subset(tweetsSparse, split==FALSE)

head(trainSparse,7)[1:7]

# Video 7

# Build a CART model

library(rpart)
library(rpart.plot)

tweetCART = rpart(Negative ~ ., data=trainSparse, method="class")

prp(tweetCART)

# Evaluate the performance of the model
predictCART = predict(tweetCART, newdata=testSparse, type="class")

table(testSparse$Negative, predictCART)

# Compute accuracy

(294+18)/(294+6+37+18)
(277+22)/(277+7+49+22)
# Baseline accuracy 

table(testSparse$Negative)

300/(300+55)


# Random forest model

library(randomForest)
set.seed(123)

tweetRF = randomForest(Negative ~ ., data=trainSparse)

# Make predictions:
predictRF = predict(tweetRF, newdata=testSparse)

table(testSparse$Negative, predictRF)

# Accuracy:
(293+21)/(293+7+34+21)


#----------------------------------------------------------------------
()/()
library(dplyr)
trainSparse$Neg <- as.factor(trainSparse$Negative)
trainSparse <- within(trainSparse,{
  Neg = NA
  Neg[Negative == TRUE] <- 1
  Neg[Negative == FALSE] <- 0})

trainSparse$Neg <- as.factor(trainSparse$Negative)
trainSparse <- within(trainSparse,{
  Neg = NA
  Neg[Negative == TRUE] <- 1
  Neg[Negative == FALSE] <- 0})
testSparse <- within(testSparse,{
  Neg = NA
  Neg[Negative == TRUE] <- 1
  Neg[Negative == FALSE] <- 0})

tweetLog <- glm(Neg~., data = trainSparse[-310], family = binomial)
predictions = predict(tweetLog, newdata=testSparse, type="response")
predictions <- ifelse(predictions >= 0.5, 1, 0)
conf <-table(testSparse$Neg, predictions)  
accuracy <- (conf[1,1]+conf[2,2])/sum(conf)
accuracy

#---------------------------------------
positive<- scan("positive.txt", 
                what= "character", 
                comment.char = ";")

negative<- scan("negative.txt", 
                what= "character", 
                comment.char = ";")

tweet <- tweets

#tweet$Tweet <- VCorpus(VectorSource(tweets$Tweet)) # create corpus
#tweet$Tweet[[1]]$content
library(stringr); library(tm); library(splitstackshape)
tweet$Tweet <- tolower(tweet$Tweet) # convert to lower case
tweet$Tweet <- removePunctuation(tweet$Tweet) # remove punctuation
tweet$Tweet <- str_replace_all(tweet$Tweet,"\\d", " ") # clean digits
tweet$Tweet <- removeWords(tweet$Tweet, c("apple", stopwords("english"))) # remove stop words
tweet$Tweet <- stemDocument(tweet$Tweet, "english") # stem document using porter's 
tweet$Tweet <- gsub(tweet$Tweet, pattern ="\\b[A-z]\\b{1}", replace =" ") # remove single letters
x <- cSplit(indt = tweet, splitCols = "Tweet", sep = " ", stripWhite = T) # split the words in different collumns
head(x,1)
class(x)
View(x)
for(i in 1:nrow(x)){
  pscore <- sum(!is.na(match(unlist(x[i,3:ncol(x)]), positive))) 
  tweet$pscore[i] = pscore}

table(tweet$pscore)

for(i in 1:nrow(x)){
  nscore <- sum(!is.na(match(unlist(x[i,3:ncol(x)]), negative)))
  tweet$nscore[i] = nscore}
table(tweet$nscore)

x$pscore <- tweet$pscore
x$nscore <- tweet$nscore*(-1)
table(x$pscore)
table(x$nscore)
x$Neg <- x$pscore+x$nscore
x$Neg <- ifelse(x$Neg >= 0, 0, 1)
table(x$Neg)

split= sample.split(x$Neg, SplitRatio = 0.7)
xtrain = subset(x, split == TRUE)
xtest = subset(x,split == FALSE)
xtrain <- xtrain[,-c(1,2)]
xtest <- xtest[,-c(1,2)]
table(x$Negative, x$Neg)

View(xtrain)

library(rpart)
library(rpart.plot)

tweetCART = rpart(Neg ~ ., data=xtrain, method="class")
prp(tweetCART)

# Evaluate the performance of the model
predictCART = predict(tweetCART, newdata=xtest, type="class")

table(xtest$Neg, predictCART)

# Compute accuracy

(294+18)/(294+6+37+18)
(251+27)/(251+33+27+44)
(250+15)/(250+34+56+15)
