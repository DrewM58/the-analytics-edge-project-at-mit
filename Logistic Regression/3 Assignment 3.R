# Week 3 Assignment
# PREDICTING PAROLE VIOLATORS
# set working directory to read data
setwd("D:/1 MOOC of Coursera edX Udacity/22 MITx 15.071x The Analytics Edge/Week 3 Logistic Regression")

# PROBLEM 1.1 - LOADING THE DATASET
parole <- read.csv("parole.csv")
str(parole)
summary(parole)

# PROBLEM 1.2 - LOADING THE DATASET
table(parole$violator)

# PROBLEM 1.3 - LOADING THE DATASET
# 4, 8
str(parole)
table(parole$male)
table(parole$race)
table(parole$age)
table(parole$state)
table(parole$time.served)
table(parole$time.served)
table(parole$multiple.offenses)
table(parole$crime)
table(parole$violator)

# PROBLEM 2.1 - PREPARING THE DATASET
summary(parole)
parole$state <- as.factor(parole$state)
parole$multiple.offenses <- as.factor(parole$multiple.offenses)
summary(parole)

# PROBLEM 2.2 - PREPARING THE DATASET
# 2

# PROBLEM 3.1 - SPLITTING INTO A TRAINING AND TESTING SET
# 1

set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)

# PROBLEM 3.2 - SPLITTING INTO A TRAINING AND TESTING SET
# 1, 2, 2

# PROBLEM 4.1 - BUILDING A LOGISTIC REGRESSION MODEL
set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)

m1 <- glm(violator ~ . , data = train, family = "binomial")
summary(m1)

# PROBLEM 4.2 - BUILDING A LOGISTIC REGRESSION MODEL
# 4???
oddsRatio <- exp(1.593069)
oddsRatio

# PROBLEM 4.3 - BUILDING A LOGISTIC REGRESSION MODEL?>?????????????
str(train)
summary(m1)
logOdd = -4.021538 + 1*0.268037 + 1*0.876409 + 0.001118*50
	+ 0.860172 + 3*(-0.126714) + 12*0.079950 + 0 + 2*(-0.056214)
Odd = exp(logOdd) 
Odd
# p / (1-p) = Odd
p = 1/ (1 + 1/Odd)
p


# PROBLEM 5.1 - EVALUATING THE MODEL ON THE TESTING SET
predTest <- predict(m1, newdata = test, type = "response")
max(predTest)




