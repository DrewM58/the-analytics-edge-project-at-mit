# VISUALIZING TEXT DATA USING WORD CLOUDS
setwd("/Users/huihuiduan/1_MOOC_of_Coursera_edX_Udacity/22 MITx 15.071x The Analytics Edge/Week 8 Visualization")
# PROBLEM 1.1 - PREPARING THE DATA
tweets <- read.csv("tweets.csv", stringsAsFactors = FALSE)
str(tweets)

install.packages("tm")
library(tm)
install.packages("SnowballC")
library(SnowballC)

# 1) Create a corpus using the Tweet variable
corpus = Corpus(VectorSource(tweets$Tweet))
# 2) Convert the corpus to lowercase
corpus = tm_map(corpus, tolower)
# 3) Remove punctuation from the corpus
corpus = tm_map(corpus, removePunctuation)
# 4) Remove all English-language stopwords
corpus = tm_map(corpus, removeWords, stopwords("english"))
# 5) Build a document-term matrix out of the corpus
frequencies = DocumentTermMatrix(corpus)
# 6) Convert the document-term matrix to a data frame called allTweets
allTweets = as.data.frame(as.matrix(frequencies))

str(allTweets)
dim(allTweets)

# PROBLEM 1.2 - PREPARING THE DATA
2

# PROBLEM 2.1 - BUILDING A WORD CLOUD
install.packages("wordcloud")
library(wordcloud)
?wordcloud
str(colnames(allTweets))

# PROBLEM 2.2 - BUILDING A WORD CLOUD
str(colSums(allTweets))

# PROBLEM 2.3 - BUILDING A WORD CLOUD
wordcloud(words = colnames(allTweets), freq = colSums(allTweets))

wordcloud(words = colnames(allTweets), 
          freq = colSums(allTweets), scale=c(2, 0.25))

# PROBLEM 2.4 - BUILDING A WORD CLOUD

# 1) Create a corpus using the Tweet variable
corpus = Corpus(VectorSource(tweets$Tweet))
# 2) Convert the corpus to lowercase
corpus = tm_map(corpus, tolower)
# 3) Remove punctuation from the corpus
corpus = tm_map(corpus, removePunctuation)
# 4) Remove all English-language stopwords
corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))
# 5) Build a document-term matrix out of the corpus
frequencies = DocumentTermMatrix(corpus)
# 6) Convert the document-term matrix to a data frame called allTweets
allTweets = as.data.frame(as.matrix(frequencies))

wordcloud(words = colnames(allTweets), freq = colSums(allTweets))

# PROBLEM 3 - SIZE AND COLOR
# PROBLEM 3.1 - SIZE AND COLOR
str(tweets)
# 1) Create a corpus using the Tweet variable
corpusNeg = Corpus(VectorSource(subset(tweets, Avg <= -1)$Tweet))
# 2) Convert the corpus to lowercase
corpusNeg = tm_map(corpusNeg, tolower)
# 3) Remove punctuation from the corpus
corpusNeg = tm_map(corpusNeg, removePunctuation)
# 4) Remove all English-language stopwords
corpusNeg = tm_map(corpusNeg, removeWords, c("apple", stopwords("english")))
# 5) Build a document-term matrix out of the corpus
frequenciesNeg = DocumentTermMatrix(corpusNeg)
# 6) Convert the document-term matrix to a data frame called allTweets
allTweetsNeg = as.data.frame(as.matrix(frequenciesNeg))

wordcloud(words = colnames(allTweetsNeg), freq = colSums(allTweetsNeg),
          random.order = TRUE, random.color = FALSE)

# PROBLEM 3.2 - SIZE AND COLOR
# A
wordcloud(words = colnames(allTweets), freq = colSums(allTweets))



# PROBLEM 3.3 - SIZE AND COLOR
wordcloud(words = colnames(allTweets), freq = colSums(allTweets),
          random.order = FALSE)

# PROBLEM 3.4 - SIZE AND COLOR
# A
wordcloud(words = colnames(allTweets), freq = colSums(allTweets),
          rot.per = 0.0001)
wordcloud(words = colnames(allTweets), freq = colSums(allTweets),
          rot.per = 1)




# PROBLEM 3.5 - SIZE AND COLOR
# C

# PROBLEM 4.1 - SELECTING A COLOR PALETTE
install.packages("RColorBrewer")
library(RColorBrewer)
?brewer.pal
?display.brewer.all
display.brewer.all()
?wordcloud
wordcloud(words = colnames(allTweets), freq = colSums(allTweets), 
          colors = brewer.pal(n = 8, name = "Accent"))
wordcloud(words = colnames(allTweets), freq = colSums(allTweets), 
          colors = brewer.pal(n = 8, name = "Set2"))
# good good good
wordcloud(words = colnames(allTweets), freq = colSums(allTweets), 
          colors = brewer.pal(n = 8, name = "YlOrRd"))

# PROBLEM 4.2 - SELECTING A COLOR PALETTE
display.brewer.all()
# Greys

# PROBLEM 4.3 - SELECTING A COLOR PALETTE
wordcloud(words = colnames(allTweets), freq = colSums(allTweets), 
          colors = brewer.pal(9, "Blues"))
wordcloud(words = colnames(allTweets), freq = colSums(allTweets), 
          colors = brewer.pal(9, "Blues")[c(-1, -2, -3, -4)])
wordcloud(words = colnames(allTweets), freq = colSums(allTweets), 
          colors = brewer.pal(9, "Blues")[c(5, 6, 7, 8, 9)])
