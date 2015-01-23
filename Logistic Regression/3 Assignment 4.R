# Week 3 Assignment
# PREDICTING PAROLE VIOLATORS
# set working directory to read data
setwd("D:/1 MOOC of Coursera edX Udacity/22 MITx 15.071x The Analytics Edge/Week 3 Logistic Regression")

# PROBLEM 1.1 - PREPARING THE DATASET
loans = read.csv("loans.csv")
str(loans)
summary(loans)
table(loans$not.fully.paid)
1533/(1533+8045)

# PROBLEM 1.2 - PREPARING THE DATASET
summary(loans)

#¡¡PROBLEM 1.3 - PREPARING THE DATASET
# 2
missing = subset(loans, is.na(log.annual.inc) | 
	is.na(days.with.cr.line) | 
	is.na(revol.util) | 
	is.na(inq.last.6mths) | 
	is.na(delinq.2yrs) | 
	is.na(pub.rec))
str(missing)

# ROBLEM 1.4 - PREPARING THE DATASET
# 3
library(mice)

set.seed(144)

vars.for.imputation = setdiff(names(loans), "not.fully.paid")

imputed = complete(mice(loans[vars.for.imputation]))

loans[vars.for.imputation] = imputed

loans <- read.csv("loans_imputed.csv")

# PROBLEM 2.1 - PREDICTION MODELS
set.seed(144)
library(caTools)
split = sample.split(loans$not.fully.paid, SplitRatio = 0.7)
train = subset(loans, split == TRUE)
test = subset(loans, split == FALSE)

m1 <- glm(not.fully.paid ~ ., data = train, family = "binomial")
summary(m1)


# PROBLEM 2.2 - PREDICTION MODELS
700*(-0.009317) - 710*(-0.009317)
exp(700*(-0.009317) - 710*(-0.009317))

# PROBLEM 2.3 - PREDICTION MODELS
test$predicted.risk <- predict(m1, newdata = test, type = "response")
table(test$not.fully.paid, test$predicted.risk >= 0.5)
(2400+3) / (2400+3+13+457)

# baseline
table(test$not.fully.paid)
2413/(2413+460)

# PROBLEM 2.4 - PREDICTION MODELS
library(ROCR)
ROCRpred = prediction(test$predicted.risk, test$not.fully.paid)
as.numeric(performance(ROCRpred, "auc")@y.values)

# PROBLEM 3.1 - A "SMART BASELINE"
m2 <- glm(not.fully.paid ~ int.rate, data = train, family = "binomial")
summary(m2)

# PROBLEM 3.2 - A "SMART BASELINE"
predTestM2 <- predict(m2, newdata = test, type = "response")

max(predTestM2)
table(test$not.fully.paid, predTestM2 >= 0.5)

# PROBLEM 3.3 - A "SMART BASELINE"  
ROCRpred = prediction(predTestM2, test$not.fully.paid)
as.numeric(performance(ROCRpred, "auc")@y.values)

# PROBLEM 4.1 - COMPUTING THE PROFITABILITY OF AN INVESTMENT
10 * exp(0.06 * 3)

# PROBLEM 4.2 - COMPUTING THE PROFITABILITY OF AN INVESTMENT
# 1

# PROBLEM 4.3 - COMPUTING THE PROFITABILITY OF AN INVESTMENT
# 4

# PROBLEM 5.1 - A SIMPLE INVESTMENT STRATEGY
test$profit = exp(test$int.rate*3) - 1

test$profit[test$not.fully.paid == 1] = -1
max(test$profit) * 10

#¡¡PROBLEM 6.1 - AN INVESTMENT STRATEGY BASED ON RISK
mean(test$profit) 
str(test)
highInterest <- subset(test, int.rate >= 0.15)
mean(highInterest$profit)
table(highInterest$not.fully.paid)
110/(110+327)


# PROBLEM 6.2 - AN INVESTMENT STRATEGY BASED ON RISK?????????????
cutoff = sort(highInterest$predicted.risk, decreasing=FALSE)[100]
selectedLoans <- subset(highInterest, highInterest$predicted.risk <= cutoff)
str(selectedLoans)
sum(selectedLoans$profit)
table(selectedLoans$not.fully.paid)






