# AUTOMATING REVIEWS IN MEDICINE

rm(list = ls())

# PROBLEM 1.1 - LOADING THE DATA
trials <- read.csv("clinical_trial.csv", stringsAsFactors=FALSE)
str(trials)
summary(trials)
nchar(trials$title)
max(nchar(trials$abstract))

# PROBLEM 1.2 - LOADING THE DATA
str(nchar(trials$abstract))
str(which(nchar(trials$abstract) == 0))
length(which(nchar(trials$abstract) == 0))

# PROBLEM 1.3 - LOADING THE DATA
# have no idea, lol

# PROBLEM 2.1 - PREPARING THE CORPUS
library(tm)
library(SnowballC)

# 1) Convert the title variable to corpusTitle and the abstract variable to corpusAbstract.
corpusTitle <- Corpus(VectorSource(trials$title))
corpusAbstract <- Corpus(VectorSource(trials$abstract))

# 2) Convert corpusTitle and corpusAbstract to lowercase.
corpusTitle <- tm_map(corpusTitle, content_transformer(tolower))
corpusAbstract <- tm_map(corpusAbstract, content_transformer(tolower))

# 3) Remove the punctuation in corpusTitle and corpusAbstract.
corpusTitle <- tm_map(corpusTitle, removePunctuation)
corpusAbstract <- tm_map(corpusAbstract, removePunctuation)

# 4) Remove the English language stop words from corpusTitle and corpusAbstract.
corpusTitle = tm_map(corpusTitle, removeWords, stopwords("english"))
corpusAbstract = tm_map(corpusAbstract, removeWords, stopwords("english"))

# 5) Stem the words in corpusTitle and corpusAbstract (each stemming might take a few minutes).
corpusTitle = tm_map(corpusTitle, stemDocument, lazy=TRUE)
corpusAbstract = tm_map(corpusAbstract, stemDocument, lazy=TRUE)

# 6) Build a document term matrix called dtmTitle from corpusTitle and dtmAbstract from corpusAbstract.
dtmTitle = DocumentTermMatrix(corpusTitle)
dtmAbstract = DocumentTermMatrix(corpusAbstract)

# 7) Limit dtmTitle and dtmAbstract to terms with sparseness of at most 95% (aka terms that appear in at least 5% of documents).
dtmTitle = removeSparseTerms(dtmTitle, 0.95)
dtmAbstract = removeSparseTerms(dtmAbstract, 0.95)

# 2.1
dtmTitle
dtmAbstract

# 8) Convert dtmTitle and dtmAbstract to data frames.
dtmTitle = as.data.frame(as.matrix(dtmTitle))
dtmAbstract = as.data.frame(as.matrix(dtmAbstract))



# PROBLEM 2.2 - PREPARING THE CORPUS
# MLGB, answer is 1, not 2

# PROBLEM 2.3 - PREPARING THE CORPUS
# why 8381 is wrong? WORD OF patient, NOT NUMBER
sort((colSums(dtmAbstract)))

#PROBLEM 3.1 - BUILDING A MODEL
colnames(dtmTitle) = paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))
str(dtmTitle)

#??PROBLEM 3.2 - BUILDING A MODEL
dtm <- cbind(dtmTitle, dtmAbstract)
# Add dependent variable
str(trials)
dtm$trial <- trials$trial
str(dtm)

# PROBLEM 3.3 - BUILDING A MODEL
# Split the data
library(caTools)
set.seed(144)
split = sample.split(dtm$trial, SplitRatio = 0.7)

trainSparse = subset(dtm, split==TRUE)
testSparse = subset(dtm, split==FALSE)
# 3.3
table(trainSparse$trial)
730/(730+572)

# PROBLEM 3.4 - BUILDING A MODEL
library(rpart)
library(rpart.plot)
trialCART = rpart(trial ~ ., data = trainSparse, method = "class")
prp(trialCART)

# PROBLEM 3.5 - BUILDING A MODEL
trailPred <- predict(trialCART)
str(trailPred)
max(trailPred[, 2])

# PROBLEM 3.6 - BUILDING A MODEL
# Evaluate the performance of the model
predictCART = predict(trialCART, newdata=testSparse)
str(predictCART)
max(predictCART[, 2])


# PROBLEM 3.7 - BUILDING A MODEL
table(trainSparse$trial, trailPred[, 2] >= 0.5)
(631+441)/(631+99+131+441)
441/(441+131)
631/(631+99)

# PROBLEM 4.1 - EVALUATING THE MODEL ON THE TESTING SET
predTest = predict(trialCART, newdata = testSparse, type="class")
table(testSparse$trial, predTest)
(261+162)/(261+52+83+162)


# PROBLEM 4.2 - EVALUATING THE MODEL ON THE TESTING SET
# ROC curve

library(ROCR)
predROCR = prediction(predictCART[, 2], testSparse$trial)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)
# Compute AUC
performance(predROCR, "auc")@y.values

# PROBLEM 5.1 - DECISION-MAKER TRADEOFFS
# 3

# PROBLEM 5.2 - DECISION-MAKER TRADEOFFS
# 1

# PROBLEM 5.3 - DECISION-MAKER TRADEOFFS
# 4, not 1, OMG





