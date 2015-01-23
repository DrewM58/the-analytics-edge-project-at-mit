# VISUALIZING ATTRIBUTES OF PAROLE VIOLATORS
setwd("/Users/huihuiduan/1_MOOC_of_Coursera_edX_Udacity/22 MITx 15.071x The Analytics Edge/Week 8 Visualization")
# PROBLEM 1.1 - LOADING THE DATA
parole <- read.csv("parole.csv")
parole$male = as.factor(parole$male)

parole$state = as.factor(parole$state)

parole$crime = as.factor(parole$crime)
str(parole)
table(parole$male, parole$violator)
14/(14+64)

# PROBLEM 1.2 - LOADING THE DATA
table(parole$crime, parole$state)

# PROBLEM 2.1 - CREATING A BASIC HISTOGRAM
ggplot(data = parole, aes(x = age)) + geom_histogram()
ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth = 5)

# PROBLEM 2.2 - CREATING A BASIC HISTOGRAM
ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth = 5, color="blue")
?geom_histogram

# PROBLEM 3.1 - ADDING ANOTHER DIMENSION
ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth = 5) + facet_grid(male ~ .)

# PROBLEM 3.2 - ADDING ANOTHER DIMENSION
ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth = 5) + facet_grid(. ~ male)

# PROBLEM 3.3 - ADDING ANOTHER DIMENSION
ggplot(data = parole, aes(x = age, fill = male)) + geom_histogram(binwidth = 5)


# PROBLEM 3.4 - ADDING ANOTHER DIMENSION
ggplot(data = parole, aes(x = age, fill = male)) + 
  geom_histogram(binwidth = 5, position = "identity", alpha=0.5)

# PROBLEM 4.1 - TIME SERVED
str(parole)
ggplot(data = parole, aes(x = time.served, fill = male)) + 
  geom_histogram(binwidth = 1, position = "identity", alpha=0.5)

# PROBLEM 4.2 - TIME SERVED
ggplot(data = parole, aes(x = time.served, fill = male)) + 
  geom_histogram(binwidth = 0.1, position = "identity", alpha=0.5)

# PROBLEM 4.3 - TIME SERVED
ggplot(data = parole, aes(x = time.served, fill = male)) + 
  geom_histogram(binwidth = 1, position = "identity", alpha=0.5) + 
  facet_grid(crime ~ .)


# PROBLEM 4.4 - TIME SERVED
ggplot(data = parole, aes(x = time.served, fill = crime)) + 
  geom_histogram(binwidth = 1, position = "identity", alpha=0.5)










