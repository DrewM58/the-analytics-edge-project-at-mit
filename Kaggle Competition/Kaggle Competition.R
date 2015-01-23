

# Read in data
setwd("/Users/huihuiduan/1_MOOC_of_Coursera_edX_Udacity/22 MITx 15.071x The Analytics Edge/Week 7 Competition")
train2 <- read.csv("traind.csv")
str(train2)
summary(train2)
test2 <- read.csv("testd.csv")
str(test2)
summary(test2)
table(train2$Happy)

# fit logistic regression as baseline
logitM1 <- glm(Happy ~ . - UserID - YOB - votes, data = train2, family = "binomial")
summary(logitM1)



logitPredTest <- predict(logitM1, newdata = test2, type = "response")
str(logitPredTest)

output <- data.frame(cbind(test2$UserID, logitPredTest))
str(output)
head(output)
names(output) <- c("UserID", "Probability1")

write.table(output, file = "logitM1.csv", quote = FALSE, sep = ",", row.names = FALSE)

# stepwise of logistic
?step
stepLogiitM1 <- step(logitM1, direction = "both")



# subset model from logistic where p values < 0.05

logitM2 <- glm(Happy ~ HouseholdStatus + 
                 EducationLevel + Party + Q122769 + Q121700 + 
                 Q121011 + Q120194 + Q120012 + Q120014 + 
                 Q119334 + Q118237 + Q116797 + Q116441 + 
                 Q116197 + Q115610 + Q115899 + Q114961 + 
                 Q113992 + Q108617 + Q108343 + Q107869 + 
                 Q106389 + Q102906 + Q102674 + Q102687 + 
                 Q102289 + Q101162 + Q100680 + Q100562 + 
                 Q99716  + Q98869, data = train2, family = "binomial")
summary(logitM2)


logitPredTest2 <- predict(logitM2, newdata = test2, type = "response")
str(logitPredTest2)

output2 <- data.frame(cbind(test2$UserID, logitPredTest2))
str(output2)
head(output2)
names(output2) <- c("UserID", "Probability1")

write.table(output2, file = "logitM2.csv", quote = FALSE, sep = ",", row.names = FALSE)

# random forest
install.packages("randomForest")
library(randomForest)
set.seed(1)
happyForest = randomForest(Happy ~ HouseholdStatus + 
                             EducationLevel + Party + Q122769 + Q121700 + 
                             Q121011 + Q120194 + Q120012 + Q120014 + 
                             Q119334 + Q118237 + Q116797 + Q116441 + 
                             Q116197 + Q115610 + Q115899 + Q114961 + 
                             Q113992 + Q108617 + Q108343 + Q107869 + 
                             Q106389 + Q102906 + Q102674 + Q102687 + 
                             Q102289 + Q101162 + Q100680 + Q100562 + 
                             Q99716  + Q98869, data = train2)
str(happyForest)
PredictForest = predict(happyForest, newdata = test2)
str(PredictForest)

output3 <- data.frame(cbind(test2$UserID, PredictForest))
str(output3)
head(output3)
names(output3) <- c("UserID", "Probability1")

write.table(output3, file = "ranFst1.csv", quote = FALSE, sep = ",", row.names = FALSE)


# average of logistic and random forest
output4 <- output3$UserID
str(output4)
average <- (output$Probability1 + output2$Probability1 + output3$Probability1) / 3
str(average)
output4 <- data.frame(cbind(output4, average))

names(output4) <- c("UserID", "Probability1")
str(output4)
write.table(output4, file = "ranFst2.csv", quote = FALSE, sep = ",", row.names = FALSE)




# average two models of subseted logistic and random forest
output5 <- output3$UserID
str(output5)
average5 <- (output2$Probability1 + output$Probability1) / 2
str(average5)
output5 <- data.frame(cbind(output5, average5))

names(output5) <- c("UserID", "Probability1")
str(output5)
write.table(output5, file = "ranFst3.csv", quote = FALSE, sep = ",", row.names = FALSE)
