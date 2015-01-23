# PREDICTING MEDICAL COSTS WITH CLUSTER-THEN-PREDICT

rm(list = ls())

# PROBLEM 1.1 - PREPARING THE DATASET
claims <- read.csv("reimbursement.csv")
str(claims)

# PROBLEM 1.2 - PREPARING THE DATASET
dim(subset(claims, alzheimers == 0 & 
                   arthritis == 0 &
             cancer == 0 &
             copd == 0 &
             depression == 0 &
             diabetes == 0 &
             heart.failure == 0 &
             ihd == 0 &
             kidney == 0 &
             osteoporosis == 0 &
             stroke == 0))
(458005 - 177578) / 458005


# PROBLEM 1.3 - PREPARING THE DATASET
cor(claims[, 2:12])


# PROBLEM 1.4 - PREPARING THE DATASET
hist(claims$reimbursement2008)


# PROBLEM 1.5 - PREPARING THE DATASET
claims$reimbursement2008 = log(claims$reimbursement2008+1)

claims$reimbursement2009 = log(claims$reimbursement2009+1)

# PROBLEM 1.6 - PREPARING THE DATASET
hist(claims$reimbursement2008)
hist(claims$reimbursement2009)
sum(claims$reimbursement2009==log(1)) / 458005

# PROBLEM 2.1 - INITIAL LINEAR REGRESSION MODEL
set.seed(144)

spl = sample(1:nrow(claims), size=0.7*nrow(claims))

train = claims[spl,]

test = claims[-spl,]

lm.claims <- lm(reimbursement2009 ~ ., data = train)
summary(lm.claims)


# PROBLEM 2.2 - INITIAL LINEAR REGRESSION MODEL
testPred <- predict(lm.claims, newdata = test)
sqrt(sum((testPred - test$reimbursement2009)^2) / (length(testPred)))

# PROBLEM 2.3 - INITIAL LINEAR REGRESSION MODEL
# 4



# PROBLEM 2.4 - INITIAL LINEAR REGRESSION MODEL
sqrt(sum((mean(train$reimbursement2009) - test$reimbursement2009)^2) / (length(testPred)))


# PROBLEM 2.5 - INITIAL LINEAR REGRESSION MODEL  (1 point possible)
sqrt(sum((test$reimbursement2008 - test$reimbursement2009)^2) / (length(testPred)))


# PROBLEM 3.1 - CLUSTERING MEDICARE BENEFICIARIES
train.limited = train

train.limited$reimbursement2009 = NULL

test.limited = test

test.limited$reimbursement2009 = NULL


# PROBLEM 3.2 - CLUSTERING MEDICARE BENEFICIARIES

library(caret)

preproc = preProcess(train.limited)

train.norm = predict(preproc, train.limited)

test.norm = predict(preproc, test.limited)

summary(train.norm$arthritis)
summary(test.norm$arthritis)

# PROBLEM 3.3 - CLUSTERING MEDICARE BENEFICIARIES
# 2

# PROBLEM 3.4 - CLUSTERING MEDICARE BENEFICIARIES
set.seed(144)
km <- kmeans(train.norm, centers = 3)
tapply(train.norm$age, km$cluster, mean)
tapply(train.norm$stroke, km$cluster, mean)
tapply(train.norm$reimbursement2008, km$cluster, mean)
summary(train.norm)


# PROBLEM 3.5 - CLUSTERING MEDICARE BENEFICIARIES  (1 point possible)
library(flexclust)

km.kcca = as.kcca(km, train.norm)

cluster.train = predict(km.kcca)

cluster.test = predict(km.kcca, newdata=test.norm)

table(cluster.test)


# PROBLEM 4.1 - CLUSTER-SPECIFIC PREDICTIONS
train1 <- subset(train, cluster.train == 1)
train2 <- subset(train, cluster.train == 2)
train3 <- subset(train, cluster.train == 3)

test1 <- subset(test, cluster.test == 1)
test2 <- subset(test, cluster.test == 2)
test3 <- subset(test, cluster.test == 3)

mean(train1$reimbursement2009)
mean(train2$reimbursement2009)
mean(train3$reimbursement2009)


# PROBLEM 4.2 - CLUSTER-SPECIFIC PREDICTIONS
lm1 <- lm(reimbursement2009 ~ ., data = train1)
summary(lm1)
lm2 <- lm(reimbursement2009 ~ ., data = train2)
summary(lm2)
lm3 <- lm(reimbursement2009 ~ ., data = train3)
summary(lm3)


# PROBLEM 4.3 - CLUSTER-SPECIFIC PREDICTIONS
pred.test1 <- predict(lm1, newdata = test1)
pred.test2 <- predict(lm2, newdata = test2)
pred.test3 <- predict(lm3, newdata = test3)
mean(pred.test1)
mean(pred.test2)
mean(pred.test3)


# PROBLEM 4.4 - CLUSTER-SPECIFIC PREDICTIONS
sqrt(sum((pred.test1 - test1$reimbursement2009)^2) / length(pred.test1))
sqrt(sum((pred.test2 - test2$reimbursement2009)^2) / length(pred.test2))
sqrt(sum((pred.test3 - test3$reimbursement2009)^2) / length(pred.test3))


sqrt(mean((pred.test1 - test1$reimbursement2009)^2))

sqrt(mean((pred.test2 - test2$reimbursement2009)^2))

sqrt(mean((pred.test3 - test3$reimbursement2009)^2))

# PROBLEM 4.5 - CLUSTER-SPECIFIC PREDICTIONS
all.predictions = c(pred.test1, pred.test2, pred.test3)

all.outcomes = c(test1$reimbursement2009, test2$reimbursement2009, test3$reimbursement2009)

sqrt(mean((all.outcomes - all.predictions)^2))















