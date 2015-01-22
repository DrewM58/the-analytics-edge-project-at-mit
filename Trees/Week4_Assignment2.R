# LETTER RECOGNITION

# PROBLEM 1.1 - PREDICTING B OR NOT B

letters <- read.csv("letters_ABPR.csv")
str(letters)
letters$isB = as.factor(letters$letter == "B")

library(caTools)
set.seed(1000)
split = sample.split(letters$isB, SplitRatio = 0.5)
train = subset(letters, split==TRUE)
test = subset(letters, split==FALSE)

table(train$isB)
1175/(1175+383)


# PROBLEM 1.2 - PREDICTING B OR NOT B
library("rpart")
CARTb = rpart(isB ~ . - letter, data=train, method="class")
predtest <- predict(CARTb, newdata = test, type = "class")

table(test$isB, predtest)
(1118+340)/(1118+57+43+340)

# PROBLEM 1.3 - PREDICTING B OR NOT B
library(randomForest)
set.seed(1000)
randomForest = randomForest(isB ~ . - letter, data=train)
predtestRF <- predict(randomForest, newdata = test, type = "class")

table(test$isB, predtestRF)
(1165+374)/(1165+374+9+10)



# PROBLEM 2.1 - PREDICTING THE LETTERS A, B, P, R
letters$letter = as.factor( letters$letter )

set.seed(2000)
split = sample.split(letters$letter, SplitRatio = 0.5)
train = subset(letters, split==TRUE)
test = subset(letters, split==FALSE)

table(letters$letter)
803/(789+766+803+758)


# PROBLEM 2.2 - PREDICTING THE LETTERS A, B, P, R
CARTb = rpart(letter ~ . - isB, data=train, method="class")
predtest <- predict(CARTb, newdata = test, type = "class")
table(predtest)
conmat <- table(test$letter, predtest)
(conmat[1,1] + conmat[2,2] + conmat[3,3] + conmat[4,4])/nrow(test)



# PROBLEM 2.3 - PREDICTING THE LETTERS A, B, P, R
set.seed(1000)
randomForest = randomForest(letter ~ . - isB, data=train)
predtestRF <- predict(randomForest, newdata = test, type = "class")

conmatRF <- table(test$letter, predtestRF)
(conmatRF[1,1] + conmatRF[2,2] + conmatRF[3,3] + conmatRF[4,4])/nrow(test)

