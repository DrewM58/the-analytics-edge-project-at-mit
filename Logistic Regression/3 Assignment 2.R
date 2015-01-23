# Week 3 Assignment
# PREDICTING THE BASEBALL WORLD SERIES CHAMPION
# set working directory to read data
setwd("D:/1 MOOC of Coursera edX Udacity/22 MITx 15.071x The Analytics Edge/Week 3 Logistic Regression")

# PROBLEM 1.1 - LIMITING TO TEAMS MAKING THE PLAYOFFS
baseball = read.csv("baseball.csv")
str(baseball) # total number of observations

# PROBLEM 1.2 - LIMITING TO TEAMS MAKING THE PLAYOFFS
table(baseball$Year)

# PROBLEM 1.3 - LIMITING TO TEAMS MAKING THE PLAYOFFS
baseball = subset(baseball, Playoffs == 1)
str(baseball)

# PROBLEM 1.4 - LIMITING TO TEAMS MAKING THE PLAYOFFS
table(baseball$Year)

# PROBLEM 2.1 - ADDING AN IMPORTANT PREDICTOR
PlayoffTable = table(baseball$Year)
PlayoffTable
names(PlayoffTable)

# PROBLEM 2.2 - ADDING AN IMPORTANT PREDICTOR
PlayoffTable[c("1990", "2001")]

# PROBLEM 2.3 - ADDING AN IMPORTANT PREDICTOR
PlayoffTable[as.character(baseball$Year)]
baseball$NumCompetitors = PlayoffTable[as.character(baseball$Year)]

# PROBLEM 2.4 - ADDING AN IMPORTANT PREDICTOR
str(baseball)
str(subset(baseball, NumCompetitors == 8))

# PROBLEM 3.1 - BIVARIATE MODELS FOR PREDICTING WORLD SERIES WINNER
baseball$WorldSeries = as.numeric(baseball$RankPlayoffs == 1)
str(baseball)
table(baseball$WorldSeries)

# PROBLEM 3.2 - BIVARIATE MODELS FOR PREDICTING WORLD SERIES WINNER
m1 <- glm(WorldSeries ~ Year, data = baseball, family = "binomial")
summary(m1)
m2 <- glm(WorldSeries ~ RS, data = baseball, family = "binomial")
summary(m2)
m3 <- glm(WorldSeries ~ RA, data = baseball, family = "binomial")
summary(m3)
m4 <- glm(WorldSeries ~ W, data = baseball, family = "binomial")
summary(m4)
m5 <- glm(WorldSeries ~ OBP, data = baseball, family = "binomial")
summary(m5)
m6 <- glm(WorldSeries ~ SLG, data = baseball, family = "binomial")
summary(m6)
m7 <- glm(WorldSeries ~ BA, data = baseball, family = "binomial")
summary(m7)
m8 <- glm(WorldSeries ~ RankSeason, data = baseball, family = "binomial")
summary(m8)
m9 <- glm(WorldSeries ~ OOBP, data = baseball, family = "binomial")
summary(m9)
m10 <- glm(WorldSeries ~ OSLG, data = baseball, family = "binomial")
summary(m10)
m11 <- glm(WorldSeries ~ NumCompetitors, data = baseball, family = "binomial")
summary(m11)
m12 <- glm(WorldSeries ~ League, data = baseball, family = "binomial")
summary(m12)

# PROBLEM 4.1 - MULTIVARIATE MODELS FOR PREDICTING WORLD SERIES WINNER
m <- glm(WorldSeries ~ Year + RA + RankSeason + NumCompetitors, 
			 data = baseball, family = "binomial")
summary(m)

# PROBLEM 4.2 - MULTIVARIATE MODELS FOR PREDICTING WORLD SERIES WINNER
cor(baseball[c("Year", "RA", "RankSeason", "NumCompetitors")])

# PROBLEM 4.3 - MULTIVARIATE MODELS FOR PREDICTING WORLD SERIES WINNER
aic <- rep(0, 10) # repeat 0 ten times to store AIC

md1 <- glm(WorldSeries ~ Year, data = baseball, family = "binomial")
aic[1] = summary(md1)$aic

md2 <- glm(WorldSeries ~ RA, data = baseball, family = "binomial")
aic[2] = summary(md2)$aic

md3 <- glm(WorldSeries ~ RankSeason, data = baseball, family = "binomial")
aic[3] = summary(md3)$aic

md4 <- glm(WorldSeries ~ NumCompetitors, data = baseball, family = "binomial")
aic[4] = summary(md4)$aic

md5 <- glm(WorldSeries ~ Year + RA, data = baseball, family = "binomial")
aic[5] = summary(md5)$aic

md6 <- glm(WorldSeries ~ Year + RankSeason, data = baseball, family = "binomial")
aic[6] = summary(md6)$aic

md7 <- glm(WorldSeries ~ Year + NumCompetitors, data = baseball, family = "binomial")
aic[7] = summary(md7)$aic

md8 <- glm(WorldSeries ~ RA + RankSeason, data = baseball, family = "binomial")
aic[8] = summary(md8)$aic

md9 <- glm(WorldSeries ~ RA + NumCompetitors, data = baseball, family = "binomial")
aic[9] = summary(md9)$aic

md10 <- glm(WorldSeries ~ RankSeason + NumCompetitors, data = baseball, family = "binomial")
aic[10] = summary(md10)$aic
aic
which.min(aic)