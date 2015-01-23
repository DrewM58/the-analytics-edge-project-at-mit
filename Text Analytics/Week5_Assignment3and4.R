# SEPARATING SPAM FROM HAM

rm(list = ls())

# PROBLEM 1.1 - LOADING THE DATASET
emails <- read.csv("emails.csv", stringsAsFactors = FALSE)
str(emails)

# PROBLEM 1.2 - LOADING THE DATASET
table(emails$spam)

# PROBLEM 1.3 - LOADING THE DATASET
str(emails$text)
str(emails$text[1])
substr(emails$text, 1, 10)

# PROBLEM 1.4 - LOADING THE DATASET
# 2

# PROBLEM 1.5 - LOADING THE DATASET
nchar(emails$text)
max(nchar(emails$text))

# PROBLEM 1.6 - LOADING THE DATASET
which.min(nchar(emails$text))



# PROBLEM 2.1 - PREPARING THE CORPUS
# 1) Build a new corpus variable called corpus.
library(tm)
library(SnowballC)
corpus <- Corpus(VectorSource(emails$text))


# 2) Using tm_map, convert the text to lowercase.
corpus <- tm_map(corpus, content_transformer(tolower))

# 3) Using tm_map, remove all punctuation from the corpus.
corpus <- tm_map(corpus, removePunctuation)

# 4) Using tm_map, remove all English stopwords from the corpus.
corpus = tm_map(corpus, removeWords, stopwords("english"))

# 5) Using tm_map, stem the words in the corpus.
corpus = tm_map(corpus, stemDocument)

# 6) Build a document term matrix from the corpus, called dtm.
dtm = DocumentTermMatrix(corpus)
dtm


# PROBLEM 2.2 - PREPARING THE CORPUS
spdtm = removeSparseTerms(dtm, 0.95)
spdtm


# PROBLEM 2.3 - PREPARING THE CORPUS
emailsSparse <- as.data.frame(as.matrix(spdtm))
colnames(emailsSparse) = make.names(colnames(emailsSparse))
sort(colSums(emailsSparse))

# PROBLEM 2.4 - PREPARING THE CORPUS
emailsSparse$spam <- emails$spam
str(emailsSparse)

hamEmailsSparse <- subset(emailsSparse, spam == 0)
sort(colSums(hamEmailsSparse))
sum(colSums(hamEmailsSparse) >= 5000)

# PROBLEM 2.5 - PREPARING THE CORPUS
spamEmailsSparse <- subset(emailsSparse, spam == 1)
sort(colSums(spamEmailsSparse))
sum(colSums(spamEmailsSparse) >= 1000)
# exclude to spam with 1368

# PROBLEM 2.6 - PREPARING THE CORPUS
# 2

# PROBLEM 2.7 - PREPARING THE CORPUS
# 2

# PROBLEM 3.1 - BUILDING MACHINE LEARNING MODELS
emailsSparse$spam = as.factor(emailsSparse$spam)
library(caTools)
set.seed(123)
split = sample.split(emailsSparse$spam, SplitRatio = 0.7)

train = subset(emailsSparse, split==TRUE)
test = subset(emailsSparse, split==FALSE)

################################################################################
# 1) A logistic regression model called spamLog. You may see a warning message 
# here - we'll discuss this more later.
spamLog <- glm(spam ~ . , data = train, family = "binomial")

# 2) A CART model called spamCART, using the default parameters to train the 
# model (don't worry about adding minbucket or cp). Remember to add the argument
# method="class" since this is a binary classification problem.
library(rpart)
library(rpart.plot)
spamCART <- rpart(spam ~ . , data = train, method="class")

# 3) A random forest model called spamRF, using the default parameters to train 
# the model (don't worry about specifying ntree or nodesize). Directly before 
# training the random forest model, set the random seed to 123 (even though 
# we've already done this earlier in the problem, it's important to set the 
# seed right before training the model so we all obtain the same results. Keep 
# in mind though that on certain operating systems, your results might still 
# be slightly different).
library(randomForest)
set.seed(123)
spamRF <- randomForest(spam ~ . , data = train)

# For each model, obtain the predicted spam probabilities for the training set. 
trainPredLog <- predict(spamLog, type="response")
trainPredCART <- predict(spamCART)
trainPredRF <- predict(spamRF, type="prob")

sum(trainPredLog < 0.00001)
sum(trainPredLog > 0.99999)
sum((trainPredLog >= 0.00001) & (trainPredLog <= 0.99999))


# PROBLEM 3.2 - BUILDING MACHINE LEARNING MODELS
summary(spamLog)

# PROBLEM 3.3 - BUILDING MACHINE LEARNING MODELS
prp(spamCART)

# PROBLEM 3.4 - BUILDING MACHINE LEARNING MODELS
table(train$spam, trainPredLog > 0.5)
(3052+954)/(3052+954+4)

# PROBLEM 3.5 - BUILDING MACHINE LEARNING MODELS
library(ROCR)
ROCRpredLog = prediction(trainPredLog, train$spam)
as.numeric(performance(ROCRpredLog, "auc")@y.values)


# PROBLEM 3.6 - BUILDING MACHINE LEARNING MODELS
table(train$spam, trainPredCART[, 2] > 0.5)
(2885+894)/(2885+894+64+167)
# another way
table(train$spam, predict(spamCART, type = "class"))

# PROBLEM 3.7 - BUILDING MACHINE LEARNING MODELS
ROCRpredCART = prediction(trainPredCART[, 2], train$spam)
as.numeric(performance(ROCRpredCART, "auc")@y.values)

# PROBLEM 3.8 - BUILDING MACHINE LEARNING MODELS
table(train$spam, trainPredRF[, 2] > 0.5)
(3013+914)/(3013+914+44+39)

# PROBLEM 3.9 - BUILDING MACHINE LEARNING MODELS
ROCRpredRF = prediction(trainPredRF[, 2], train$spam)
as.numeric(performance(ROCRpredRF, "auc")@y.values)

# PROBLEM 3.10 - BUILDING MACHINE LEARNING MODELS
# 1

################################################################################
PROBLEM 4.1 - EVALUATING ON THE TEST SET
testLog <- predict(spamLog, newdata = test, type = "response")
str(testLog)
testCART <- predict(spamCART, newdata = test)
str(testCART)
testRF <- predict(spamRF, newdata = test, type = "prob")
str(testRF)

table(test$spam, testLog > 0.5)
(1257+376)/(1257+376+34+51)

# PROBLEM 4.2 - EVALUATING ON THE TEST SET
ROCRpredTestLog = prediction(testLog, test$spam)
as.numeric(performance(ROCRpredTestLog, "auc")@y.values)

# PROBLEM 4.3 - EVALUATING ON THE TEST SET
table(test$spam, testCART[, 2] > 0.5)
(1228+386)/(1228+386+24+80)

# PROBLEM 4.4 - EVALUATING ON THE TEST SET
ROCRpredTestCART = prediction(testCART[, 2], test$spam)
as.numeric(performance(ROCRpredTestCART, "auc")@y.values)

# PROBLEM 4.5 - EVALUATING ON THE TEST SET
table(test$spam, testRF[, 2] > 0.5)
(1290+385)/(1290+385+25+18)

# PROBLEM 4.6 - EVALUATING ON THE TEST SET
ROCRpredTestRF = prediction(testRF[, 2], test$spam)
as.numeric(performance(ROCRpredTestRF, "auc")@y.values)

# PROBLEM 4.7 - EVALUATING ON THE TEST SET
# 3

# PROBLEM 4.8 - EVALUATING ON THE TEST SET
# 1





################################################################################
# PROBLEM 5.1 - ASSIGNING WEIGHTS TO DIFFERENT TYPES OF ERRORS
# 2
# 1

# PROBLEM 5.2 - ASSIGNING WEIGHTS TO DIFFERENT TYPES OF ERRORS
# 2

# PROBLEM 5.3 - ASSIGNING WEIGHTS TO DIFFERENT TYPES OF ERRORS
# 2

# PROBLEM 5.4 - ASSIGNING WEIGHTS TO DIFFERENT TYPES OF ERRORS
# 3

# PROBLEM 5.5 - ASSIGNING WEIGHTS TO DIFFERENT TYPES OF ERRORS
# 3

# PROBLEM 5.6 - ASSIGNING WEIGHTS TO DIFFERENT TYPES OF ERRORS
# 2



# PROBLEM 6.1 - INTEGRATING WORD COUNT INFORMATION
wordCount = rowSums(as.matrix(dtm))

# IMPORTANT NOTE: If you received an error message when running the command above,
# it might be because your computer ran out of memory when trying to convert dtm 
# to a matrix. If this happened to you, try running the following lines of code 
# instead to create wordCount (if you didn't get an error, you don't need to run 
# these lines). This code is a little more cryptic, but is more memory efficient.
# library(slam)
# wordCount = rollup(dtm, 2, FUN=sum)$v
str(spdtm)
str(dtm)
# 3

# PROBLEM 6.2 - INTEGRATING WORD COUNT INFORMATION
hist(wordCount)
# 1

# PROBLEM 6.3 - INTEGRATING WORD COUNT INFORMATION
hist(log(wordCount))
# 2

# PROBLEM 6.4 - INTEGRATING WORD COUNT INFORMATION
emailsSparse$logWordCount <- log(wordCount)
?boxplot() 
boxplot(logWordCount ~ spam, data = emailsSparse)
#??

# PROBLEM 6.5 - INTEGRATING WORD COUNT INFORMATION

# 1) Use the same sample.split output you obtained earlier 
# (do not re-run sample.split) to split emailsSparse into 
# a training and testing set, which you should call train2 and test2.
train2 = subset(emailsSparse, split==TRUE)
test2 = subset(emailsSparse, split==FALSE)

# 2) Use train2 to train a CART tree with the default parameters, 
# saving the model to the variable spam2CART.
spam2CART <- rpart(spam ~ . , data = train2, method = "class")

# 3) Use train2 to train a random forest with the default parameters, 
# saving the model to the variable spam2RF. Again, set the random seed 
# to 123 directly before training spam2RF.
set.seed(123)
spam2RF <- randomForest(spam ~ . , data = train2)

prp(spam2CART)
# 1

# PROBLEM 6.6 - INTEGRATING WORD COUNT INFORMATION
test2CART <- predict(spam2CART, newdata = test2)
str(test2CART)
table(test2$spam, test2CART[, 2] > 0.5)
(1214+384)/(1214+384+26+94)

# PROBLEM 6.7 - INTEGRATING WORD COUNT INFORMATION
ROCRpredTest2CART = prediction(test2CART[, 2], test2$spam)
as.numeric(performance(ROCRpredTest2CART, "auc")@y.values)

# PROBLEM 6.8 - INTEGRATING WORD COUNT INFORMATION
test2RF <- predict(spam2RF, newdata = test2, type = "prob")
table(test2$spam, test2RF[, 2] > 0.5)
(1298+382)/(1298+382+28+10)

# PROBLEM 6.9 - INTEGRATING WORD COUNT INFORMATION
ROCRpredTest2RF = prediction(test2RF[, 2], test2$spam)
as.numeric(performance(ROCRpredTest2RF, "auc")@y.values)

# PROBLEM 7.1 - USING 2-GRAMS TO PREDICT SPAM
#install.packages("RTextTools")
library("RTextTools")
dtm2gram = create_matrix(as.character(corpus), ngramLength=2)
dtm2gram

# PROBLEM 7.2 - USING 2-GRAMS TO PREDICT SPAM
spdtm2gram = removeSparseTerms(dtm2gram, 0.95)
spdtm2gram

# PROBLEM 7.3 - USING 2-GRAMS TO PREDICT SPAM
# 2

# PROBLEM 7.4 - USING 2-GRAMS TO PREDICT SPAM

# 1) Build data frame emailsSparse2gram from spdtm2gram, 
# using as.data.frame() and as.matrix().
emailsSparse2gram <- as.data.frame(as.matrix(spdtm2gram))

# 2) Convert the column names of emailsSparse2gram to 
# valid names using make.names().
colnames(emailsSparse2gram) = make.names(colnames(emailsSparse2gram))

# 3) Combine the original emailsSparse with emailsSparse2gram into a final 
# data frame with the command "emailsCombined = cbind(emailsSparse, emailsSparse2gram)".
emailsCombined = cbind(emailsSparse, emailsSparse2gram)

# 4) Use the same sample.split output you obtained earlier (do not re-run sample.split)
# to split emailsCombined into a training and testing set, which you should call 
# trainCombined and testCombined.
trainCombined <- subset(emailsCombined, split == TRUE)
testCombined <- subset(emailsCombined, split == FALSE)

# 5) Use trainCombined to train a CART tree with the default parameters, 
# saving the model to the variable spamCARTcombined.
spamCARTcombined <- rpart(spam ~ . , data = trainCombined, method = "class")

# 6) Use trainCombined to train a random forest with the default parameters, 
# saving the model to the variable spamRFcombined. Again, set the random 
# seed to 123 directly before training the random forest model.
set.seed(123)
spamRFcombined <- randomForest(spam ~ . , data = trainCombined)

prp(spamCARTcombined, varlen=0)

# PROBLEM 7.5 - USING 2-GRAMS TO PREDICT SPAM
spamCARTcombinedTestPred <- predict(spamCARTcombined, newdata = testCombined)
spamRFcombinedTestPred <- predict(spamRFcombined, newdata = testCombined, type = "prob")

table(testCombined$spam, spamCARTcombinedTestPred[, 2] > 0.5)
(1233+374)/(1233+374+36+75)

# PROBLEM 7.6 - USING 2-GRAMS TO PREDICT SPAM
ROCRpredTestCARTcombined = prediction(spamCARTcombinedTestPred[, 2], testCombined$spam)
as.numeric(performance(ROCRpredTestCARTcombined, "auc")@y.values)

# PROBLEM 7.7 - USING 2-GRAMS TO PREDICT SPAM
table(testCombined$spam, spamRFcombinedTestPred[, 2] > 0.5)
(1296+384)/(1296+384+26+12)

ROCRpredTestRFcombined = prediction(spamRFcombinedTestPred[, 2], testCombined$spam)
as.numeric(performance(ROCRpredTestRFcombined, "auc")@y.values)