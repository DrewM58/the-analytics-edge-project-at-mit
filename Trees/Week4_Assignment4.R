# PREDICTING EARNINGS FROM CENSUS DATA

census <- read.csv("census.csv")
str(census)
# PROBLEM 1.1 - A LOGISTIC REGRESSION MODEL
# Split the data
library(caTools)
set.seed(2000)
split = sample.split(census$over50k, SplitRatio = 0.6)
train = subset(census, split==TRUE)
test = subset(census, split==FALSE)

logm1 <- glm(over50k ~ ., data = train, 
			family = "binomial")

summary(logm1)



# PROBLEM 1.2 - A LOGISTIC REGRESSION MODEL
logm1Pred <- predict(logm1, newdata = test, 
				type = "response")
table(test$over50k, logm1Pred > 0.5)
(9051+1888)/(9051+662+1190+1888)

# PROBLEM 1.3 - A LOGISTIC REGRESSION MODEL
table(test$over50k)
9713/(9713+3078)

# PROBLEM 1.4 - A LOGISTIC REGRESSION MODEL
library(ROCR)
ROCRpred = prediction(logm1Pred, test$over50k)
as.numeric(performance(ROCRpred, "auc")@y.values)

# PROBLEM 2.1 - A CART MODEL
library(rpart)
library(rpart.plot)
censusTree = rpart(over50k ~ ., data = train, method = "class")
prp(censusTree)

# PROBLEM 2.2 - A CART MODEL 
prp(censusTree) 


# PROBLEM 2.3 - A CART MODEL
prp(censusTree) 

#??PROBLEM 2.4 - A CART MODEL
PredictCART = predict(censusTree, newdata = test, type = "class")
table(test$over50k, PredictCART)
(9243+1596)/(9243+470+1482+1596)

# PROBLEM 2.5 - A CART MODEL
# 3
library(ROCR)
PredictROC = predict(censusTree, newdata = test)
PredictROC

pred = prediction(PredictROC[,2], test$over50k)
perf = performance(pred, "tpr", "fpr")
plot(perf)


# PROBLEM 2.6 - A CART MODEL
as.numeric(performance(pred, "auc")@y.values)

# PROBLEM 3.1 - A RANDOM FOREST MODEL
set.seed(1)

trainSmall = train[sample(nrow(train), 2000), ]
library(randomForest)
# Build random forest model
censusForest = randomForest(over50k ~ ., data = trainSmall)
str(trainSmall)

# PROBLEM 3.2 - A RANDOM FOREST MODEL
set.seed(1)
censusForest = randomForest(over50k ~ . - nativecountry, data = trainSmall)

# Make predictions
PredictForest = predict(censusForest, newdata = test)
table(test$over50k, PredictForest)

(8861+2038) / (8861+852+1040+2038)



# PROBLEM 3.3 - A RANDOM FOREST MODEL
vu = varUsed(censusForest, count=TRUE)

vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)

dotchart(vusorted$x, names(censusForest$forest$xlevels[vusorted$ix]))


# PROBLEM 3.4 - A RANDOM FOREST MODEL
varImpPlot(censusForest)


# PROBLEM 4.1 - SELECTING CP BY CROSS-VALIDATION
library(caret)
library(e1071)
set.seed(2)

# Define cross-validation experiment
fitControl = trainControl( method = "cv", number = 10 )
cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002)) 

# Perform the cross validation
train(over50k ~ ., 
	data = train, method = "rpart", 
	trControl = fitControl, tuneGrid = cartGrid )

# PROBLEM 4.2 - SELECTING CP BY CROSS-VALIDATION
# Create a new CART model
censusTreeCV = rpart(over50k ~ ., 
	method="class", data = train, 
	control=rpart.control(cp = 0.002))
prp(censusTreeCV)

# Make predictions
PredictCV = predict(censusTreeCV, newdata = test, type = "class")
table(test$over50k, PredictCV)
(9178+1838)/(9178+535+1240+1838)


# PROBLEM 4.3 - SELECTING CP BY CROSS-VALIDATION
prp(censusTreeCV)
# 18




