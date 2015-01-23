# ELECTION FORECASTING REVISITED
setwd("/Users/huihuiduan/1_MOOC_of_Coursera_edX_Udacity/22 MITx 15.071x The Analytics Edge/Week 8 Visualization")

library(ggplot2)
library(maps)
library(ggmap)
statesMap = map_data("state")

# PROBLEM 1.1 - DRAWING A MAP OF THE US
str(statesMap)
table(statesMap$group)

# PROBLEM 1.2 - DRAWING A MAP OF THE US
ggplot(statesMap, aes(x = long, y = lat, group = group)) + 
  geom_polygon(fill = "white", color = "black") + coord_map("mercator")
?mapproject

ggplot(statesMap, aes(x = long, y = lat, group = group)) + 
  geom_polygon(fill = "white", color = "black") + coord_map("orthographic")
ggplot(statesMap, aes(x = long, y = lat, group = group)) + 
  geom_polygon(fill = "white", color = "black") + coord_map("polyconic")

# PROBLEM 2.1 - COLORING THE STATES BY PREDICTIONS
polling <- read.csv("PollingImputed.csv")
str(polling)
Train <- subset(polling, Year == 2004 | Year == 2008)
Test <- subset(polling, Year == 2012)
str(Train)
str(Test)
mod2 = glm(Republican~SurveyUSA+DiffCount, data=Train, family="binomial")

TestPrediction = predict(mod2, newdata=Test, type="response")
TestPredictionBinary = as.numeric(TestPrediction > 0.5)
predictionDataFrame = data.frame(TestPrediction, TestPredictionBinary, Test$State)
table(predictionDataFrame$TestPredictionBinary)
mean(predictionDataFrame$TestPrediction)

# PROBLEM 2.2 - COLORING THE STATES BY PREDICTIONS
predictionDataFrame$region = tolower(predictionDataFrame$Test.State)
predictionMap = merge(statesMap, predictionDataFrame, by = "region")
predictionMap = predictionMap[order(predictionMap$order),]
str(predictionMap)
str(statesMap)

# PROBLEM 2.3 - COLORING THE STATES BY PREDICTIONS
?merge
3

# PROBLEM 2.4 - COLORING THE STATES BY PREDICTIONS
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = 
                            TestPredictionBinary)) + 
  geom_polygon(color = "black")

# PROBLEM 2.5 - COLORING THE STATES BY PREDICTIONS
ggplot(predictionMap, aes(x = long, y = lat, group = group, 
                          fill = TestPredictionBinary)) + 
  geom_polygon(color = "black") + 
  scale_fill_gradient(low = "blue", high = "red", 
                      guide = "legend", breaks= c(0,1), 
                      labels = c("Democrat", "Republican"), 
                      name = "Prediction 2012")

ggplot(predictionMap, aes(x = long, y = lat, group = group, 
                          fill = TestPrediction)) + 
  geom_polygon(color = "black") + 
  scale_fill_gradient(low = "blue", high = "red", 
                      guide = "legend", breaks= c(0,1), 
                      labels = c("Democrat", "Republican"), 
                      name = "Prediction 2012")

# PROBLEM 3.2 - UNDERSTANDING THE PREDICTIONS


# PROBLEM 3.3 - UNDERSTANDING THE PREDICTIONS
predictionDataFrame


# PROBLEM 4 - PARAMETER SETTINGS
?geom_polygon

# PROBLEM 4.1 - PARAMETER SETTINGS  
ggplot(predictionMap, aes(x = long, y = lat, group = group, 
                          fill = TestPrediction)) + 
  geom_polygon(linetype = 3, color = "black") + 
  scale_fill_gradient(low = "blue", high = "red", 
                      guide = "legend", breaks= c(0,1), 
                      labels = c("Democrat", "Republican"), 
                      name = "Prediction 2012")

ggplot(predictionMap, aes(x = long, y = lat, group = group, 
                          fill = TestPrediction)) + 
  geom_polygon(size = 3, color = "black") + 
  scale_fill_gradient(low = "blue", high = "red", 
                      guide = "legend", breaks= c(0,1), 
                      labels = c("Democrat", "Republican"), 
                      name = "Prediction 2012")
# PROBLEM 4.2 - PARAMETER SETTINGS
ggplot(predictionMap, aes(x = long, y = lat, group = group, 
                          fill = TestPrediction)) + 
  geom_polygon(alpha = 0.3, color = "black") + 
  scale_fill_gradient(low = "blue", high = "red", 
                      guide = "legend", breaks= c(0,1), 
                      labels = c("Democrat", "Republican"), 
                      name = "Prediction 2012")
