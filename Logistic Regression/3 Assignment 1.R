# Week 3 Assignment
# POPULARITY OF MUSIC RECORDS
# set working directory to read data
setwd("D:/1 MOOC of Coursera edX Udacity/22 MITx 15.071x The Analytics Edge/Week 3 Logistic Regression")
songs <- read.csv("songs.csv")
str(songs)
# PROBLEM 1.1 - UNDERSTANDING THE DATA
songs2010 <- subset(songs, year == 2010)
str(songs2010)

# PROBLEM 1.2 - UNDERSTANDING THE DATA  
songsMJ <- subset(songs, artistname == "Michael Jackson")
str(songsMJ)

# PROBLEM 1.3 - UNDERSTANDING THE DATA
MJTop10 <- subset(songsMJ, Top10 == 1)
MJTop10$songtitle

# PROBLEM 1.4 - UNDERSTANDING THE DATA
table(songs$timesignature)

# PROBLEM 1.6 - UNDERSTANDING THE DATA
which.max(songs$tempo)
songs[6206, ]
songs[6206, ]$songtitle

# PROBLEM 2.1 - CREATING OUR PREDICTION MODEL
SongsTrain = subset(songs, year <= 2009)
SongsTest = subset(songs, year == 2010)
str(SongsTrain)

# PROBLEM 2.2 - CREATING OUR PREDICTION MODEL
# SongsLog1 = glm(Top10 ~ ., data=SongsTrain, family=binomial)
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]
model1 = glm(Top10 ~ ., data=SongsTrain, family= "binomial")
summary(model1)

# PROBLEM 2.3 - CREATING OUR PREDICTION MODEL
# coefficients > 0 will increase p(Top10 = 1)

# PROBLEM 2.4 - CREATING OUR PREDICTION MODEL
# Top10 is 1 -> higher time signature, tempo and key -> less complex

# PROBLEM 2.5 - CREATING OUR PREDICTION MODEL
# loudness                  2.999e-01
# loudness increase -> increase p(Top10 = 1)
# energy                   -1.502e+00
# energy increase -> decrease p(Top10 = 1)

# PROBLEM 3.1 - BEWARE OF MULTICOLLINEARITY ISSUES!
cor(SongsTrain$loudness, SongsTrain$energy)

# PROBLEM 3.2 - BEWARE OF MULTICOLLINEARITY ISSUES!
model2 = glm(Top10 ~ . - loudness, data=SongsTrain, family=binomial)
summary(model2)
# energy                    1.813e-01

# PROBLEM 3.3 - BEWARE OF MULTICOLLINEARITY ISSUES!
model3 = glm(Top10 ~ . - energy, data=SongsTrain, family=binomial)
summary(model3)
# loudness                  2.306e-01


# PROBLEM 4.1 - VALIDATING OUR MODEL
predictTest = predict(model3, type="response", newdata = SongsTest)
table(SongsTest$Top10, predictTest > 0.45)
(309 + 19) / (309+19+40+5)


# PROBLEM 4.2 - VALIDATING OUR MODEL
table(SongsTrain$Top10)
table(SongsTest$Top10)
314 / (314 + 59)


# PROBLEM 4.3 - VALIDATING OUR MODEL
table(SongsTest$Top10, predictTest > 0.45)

# PROBLEM 4.4 - VALIDATING OUR MODEL
19 / (40 + 19)
309 / (309 + 5)

# PROBLEM 4.5 - VALIDATING OUR MODEL
# 1, 4