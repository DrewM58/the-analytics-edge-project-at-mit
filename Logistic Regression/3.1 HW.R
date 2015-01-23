# 3.1 Modeling the Expert

# set working directory to read data
setwd("D:/1 MOOC of Coursera edX Udacity/22 MITx 15.071x The Analytics Edge/Week 3 Logistic Regression")


# QUICK QUESTION 2
# 1, 3, 4, 6
# 3, 6

# QUICK QUESTION 3
# log(Odds) = beta0 + beta1 * x1 + beta2 * x2
-1.5 + 3 * 1 + (-0.5) * 5

# Odds = exp(beta0 + beta1 * x1 + beta2 * x2)
exp(-1.5 + 3 * 1 + (-0.5) * 5)

# P(y = 1) = 1 / (1 + exp(-1 * (beta0 + beta1 * x1 + beta2 * x2) ) )
1 / (1 + exp(-1 * (-1.5 + 3 * 1 + (-0.5) * 5) ))


# QUICK QUESTION 4.1
quality = read.csv("quality.csv")

install.packages("caTools")

library(caTools)

set.seed(88)

split = sample.split(quality$PoorCare, SplitRatio = 0.75)

qualityTrain = subset(quality, split == TRUE)

qualityTest = subset(quality, split == FALSE)

# Change the Xs variables to answer questions
QualityLog = glm(PoorCare ~ StartedOnCombination + ProviderCount, data=qualityTrain, family=binomial)
summary(QualityLog)

# QUICK QUESTION 4.2
# coefficient = 1.95230 for TRUE(1), which means TRUE will increase probability of y = 1, poor care



# QUICK QUESTION 5.1
20 / (20 + 5)
15 / (15 + 10)

# QUICK QUESTION 5.2
# 5.1 to 5.2: sensitivity decrease from 0.8 to 0.6; specificity increase from 0.6 to 0.8. 
# from the video 5 example, increase threshold will have this affect. 


# QUICK QUESTION 6
# correctly identify a small group of patients who are receiving the worst care with high confidence
# = TP as large as possible = high sensitivity (y axis) with smaller false positive

# correctly identify half of the patients receiving poor care, while making as few errors as possible
# making as few errors as possible = low false positive (x axis)


# QUICK QUESTION 7
QualityLog = glm(PoorCare ~ OfficeVisits + Narcotics, data=qualityTrain, family=binomial)
summary(QualityLog)

predictTest = predict(QualityLog, type="response", newdata=qualityTest)

ROCRpredTest = prediction(predictTest, qualityTest$PoorCare)

auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc