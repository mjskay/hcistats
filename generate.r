Enter file contents here### File to generate the data used in the examples. See also: Data_overview.docx

### Libraries:
library(digest)		# for MD5 hashes for userID's
library(psych)		# for simulations
library(plyr)

### Data generating functions
source("functions.R")

### Set the seed for replication
set.seed(10)

# Settings
numberOfPersons <- 10


# Generate the UserID's
uid <- generateUIDs(numberOfPersons)

# assign to Teams
base <- assignTeam(c("Team A", "Team B"), data.frame(uid))
head(base)

# generate monthly scale data (brings data to long form):
t <- 4  # time in months
scale <- monthlySUSScale(t, base, 2, .2, .5)  # scale composed of 2 subscales
head(scale)

# generate response time (average) to customer emails ()
email <- responseTime(t, base, 20, .5, 2, 1)
head(email)

# generates average heart rate
# note that this takes a while (understatement) and gives a very large file
heart <- heartRatePerMinute(uid, months=.01)  #generates "log" data. Currently no effect of group. 


# Group users:
grouping <- generateGroups(uid, no.groupings = 4, levels = c(2,10,100,4))

# generate network between users:
network <- generateNetwork(uid, .1, FALSE)

# We might need:
# Dichotomous data
# Categorical data
# A data generating model based on groupings?


