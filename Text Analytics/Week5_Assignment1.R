# DETECTING VANDALISM ON WIKIPEDIA
rm(list = ls())

# PROBLEM 1.1 - BAGS OF WORDS

wiki <- read.csv("wiki.csv", stringsAsFactors = FALSE)
str(wiki)
wiki$Vandal = as.factor(wiki$Vandal)
str(wiki)

table(wiki$Vandal)

# PROBLEM 1.2 - BAGS OF WORDS
# 1) Create the corpus for the Added column, and call it "corpusAdded".
library(tm)
library(SnowballC)
corpusAdded = Corpus(VectorSource(wiki$Added))

# 2) Remove the English-language stopwords.
corpusAdded = tm_map(corpusAdded, removeWords, stopwords("english"))

# 3) Stem the words.
corpusAdded = tm_map(corpusAdded, stemDocument)

# 4) Build the DocumentTermMatrix, and call it dtmAdded.
dtmAdded = DocumentTermMatrix(corpusAdded)
dtmAdded


# PROBLEM 1.3 - BAGS OF WORDS
sparseAdded <- removeSparseTerms(dtmAdded, 0.997)
sparseAdded


# PROBLEM 1.4 - BAGS OF WORDS
wordsAdded <- as.data.frame(as.matrix(sparseAdded))
str(wordsAdded)
colnames(wordsAdded) = paste("A", colnames(wordsAdded))


################################################################################
# 1) Create the corpus for the Removed column, and call it "corpusRemoved".
corpusRemoved = Corpus(VectorSource(wiki$Removed))

# 2) Remove the English-language stopwords.
corpusRemoved = tm_map(corpusRemoved, removeWords, stopwords("english"))

# 3) Stem the words.
corpusRemoved = tm_map(corpusRemoved, stemDocument)

# 4) Build the DocumentTermMatrix, and call it dtmRemoved.
dtmRemoved = DocumentTermMatrix(corpusRemoved)
dtmRemoved


# PROBLEM 1.3 - BAGS OF WORDS
sparseRemoved <- removeSparseTerms(dtmRemoved, 0.997)
sparseRemoved


# PROBLEM 1.4 - BAGS OF WORDS
wordsRemoved <- as.data.frame(as.matrix(sparseRemoved))
str(wordsRemoved)
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))
################################################################################



#??PROBLEM 1.5 - BAGS OF WORDS
wikiWords <- cbind(wordsAdded, wordsRemoved)
wikiWords$Vandal <- wiki$Vandal

library(caTools)
set.seed(123)
split = sample.split(wikiWords$Vandal, SplitRatio = 0.7)

trainSparse = subset(wikiWords, split==TRUE)
testSparse = subset(wikiWords, split==FALSE)

# Use training: predict 0 (1443 0's and 1270 1's)
table(trainSparse$Vandal)

# baseline accuracy
table(testSparse$Vandal)
618/(618 + 545)

#??PROBLEM 1.6 - BAGS OF WORDS
library(rpart)
library(rpart.plot)
wikiCART = rpart(Vandal ~ ., data = trainSparse, method = "class")

predictCART = predict(wikiCART, newdata=testSparse, type="class")
table(testSparse$Vandal, predictCART)
(618+12) / (618 + 0 + 533 + 12)

# PROBLEM 1.7 - BAGS OF WORDS  
prp(wikiCART)


# PROBLEM 1.8 - BAGS OF WORDS
# 3


# PROBLEM 2.1 - PROBLEM-SPECIFIC KNOWLEDGE
grepl("cat","dogs and cats",fixed=TRUE) # TRUE

grepl("cat","dogs and rats",fixed=TRUE) # FALSE

wikiWords2 = wikiWords
wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)
table(wikiWords2$HTTP)

# PROBLEM 2.2 - PROBLEM-SPECIFIC KNOWLEDGE
wikiTrain2 = subset(wikiWords2, split == TRUE)
wikiTest2 = subset(wikiWords2, split == FALSE)

wikiCART2 = rpart(Vandal ~ ., data = wikiTrain2, method = "class")

predictCART2 = predict(wikiCART2, newdata = wikiTest2, type="class")
table(wikiTest2$Vandal, predictCART2)
(609 + 57) / (609+9+488+57)



# PROBLEM 2.3 - PROBLEM-SPECIFIC KNOWLEDGE
wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))

wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))

summary(wikiWords2$NumWordsAdded)



# PROBLEM 2.4 - PROBLEM-SPECIFIC KNOWLEDGE 
wikiTrain2 = subset(wikiWords2, split == TRUE)
wikiTest2 = subset(wikiWords2, split == FALSE)
wikiCART3 = rpart(Vandal ~ ., data = wikiTrain2, method = "class")

predictCART3 = predict(wikiCART3, newdata = wikiTest2, type="class")
table(wikiTest2$Vandal, predictCART3) 
(514+248)/(514+248+104+297)


# PROBLEM 3.1 - USING NON-TEXTUAL DATA
wikiWords3 = wikiWords2
wikiWords3$Minor = wiki$Minor

wikiWords3$Loggedin = wiki$Loggedin

wikiTrain3 = subset(wikiWords3, split == TRUE)
wikiTest3 = subset(wikiWords3, split == FALSE)
wikiCART4 = rpart(Vandal ~ ., data = wikiTrain3, method = "class")

predictCART4 = predict(wikiCART4, newdata = wikiTest3, type="class")
table(wikiTest3$Vandal, predictCART4)
(595+241)/(595+242+23+304) 


# PROBLEM 3.2 - USING NON-TEXTUAL DATA
prp(wikiCART4)
