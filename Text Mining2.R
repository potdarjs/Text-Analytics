
########## READING THE DATA ###############

setwd("E:\\Data Analytics with RET\\Text Scrapping")

readLines("Acknowledgment.txt")

# Let us have a vector of words and apply paste funtion into it

a<- c("Hello", "World", "My", "First", "Handshake", "Through", "R")
a
# We are looking to use paste function and make it a chunk

b<- paste(a, collapse = " ")
b
# Just check how do these two look
a
b

# Storing Lines in a vector named chunk 

chunk_2<- readLines("Acknowledgment.txt")
chunk_2
# and using paste funtion to store again in chunk_pasted
chunk_pasted_2 <- paste(chunk_2, collapse = " ")

#let us see how top of our data looks like
head(chunk_pasted_2)


########## CLEANING THE DATA ############

# let us now lower case this data
clean_data1<- tolower(chunk_pasted_2)
head(clean_data1)

# Cleaning the punctuations, pattern = "\\W"
# We are going to replace the punctuations by space, 
# if we don't do so then we may make new words

clean_data2 <- gsub(pattern = "\\W", replace = " ", clean_data1)
head(clean_data2)

# Cleaning the digits, pattern = "\\d"
# PROBABLY NOT REQUIRED!!

clean_data3 <- gsub(pattern = "\\d", replace = " ", clean_data2)
head(clean_data3)

# Cleaning the stopwords
# install.packages('tm')
library('tm')

#let see a preview of stopwords
stopwords()[1:10]

#let us remove them using function removeWords()
clean_data4 <- removeWords(clean_data3, stopwords())
head(clean_data4)


head(clean_data4)

# let us remove single letters, here \\b[A-z] represents that string starts 
# with any letter between a-z and string can take uppercase letters as well 
# as lower case letters and the subsequent \\b{1} says that the 
# string ends with length one

clean_data5 <- gsub(pattern = "\\b[A-z]\\b{1}", replace = " ", clean_data4)
clean_data5


# We can finally remove whitespaces using 
# stripWhitespace() function, which is part of tm library

clean_data6 <- stripWhitespace(clean_data5)

head(clean_data6)

# steming

clean_data6 <- stemDocument(clean_data6, language = "english")

head(clean_data6)

###### FREQUENCY OF THE WORDS  ############
# We now have a chunk of lines, and we are looking for counting the words
# If you remember we had joined variouslines and made a chunk
# So we split individual words and add a space between them as splitter

clean_data7<- strsplit(clean_data6," ")

word_freq1<-table(clean_data7)
head(word_freq1)


word_freq2<- cbind(names(word_freq1), as.integer(word_freq1))
head(word_freq2)

write.csv(word_freq2, "Word Frequency5.csv")

###### Word Cloud ########
# we will use word cloud library to come up with word cloud
# install.packages("RColorBrewer")
# install.packages("wordcloud")

library(RColorBrewer)
library(wordcloud)

# After installing packages, 


# we want to organize our words as per wordcloud()

class(clean_data7)

# Class of data stored as words
# We want the class to be as characters
# One way to do is to unlist the list

word_cloud1<-unlist(clean_data7)

class(word_cloud1)

wordcloud(word_cloud1) # The most raw form of wordcloud
wordcloud(word_cloud1, min.freq = 2) 
#A slightly better version
wordcloud(word_cloud1, min.freq = 2, 
          random.order = FALSE)
# We want to center the major words
# We want frequent words to be 5 times larger 
# in shape than infrequent words
wordcloud(word_cloud1, min.freq = 2, 
          random.order = FALSE, scale = c(5,1))
# We want to add colors to words, 
# we chose rainbow function to add multiple colors, 
# number of clors in parentheses
wordcloud(word_cloud1, min.freq = 2, 
        random.order = FALSE, scale = c(5,1), 
        colors = rainbow(7))


##### Sentiment Analysis  ######
# Getting the bags of positive and negative words
positive<- scan("positive.txt", 
                what= "character", 
                comment.char = ";")

negative<- scan("negative.txt", 
                what= "character", 
                comment.char = ";")

# Now we have our positive and negative words
# We will use them to match words in our text, 
# using match()

senti_analysis<- unlist(clean_data7)

match(senti_analysis, positive)[1:40]
match(senti_analysis, negative)[1:40]

# We can see the output, place where it is matching,  we have a number
# This number is representing the position of the word in the list which matches


# Now we shall count the positive and negative words Where so ever NA values are 
# not there, there is a word to be counted Counting and summing them will give total 
# number of positive score Final Sentiment score will be positive-negative score

p_score <- sum(!is.na(match(senti_analysis, positive)))
p_score
n_score <- sum(!is.na(match(senti_analysis, negative)))
n_score

Sentiment_score = p_score - n_score
Sentiment_score

