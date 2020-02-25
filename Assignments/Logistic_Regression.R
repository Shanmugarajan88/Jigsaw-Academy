#####Logistic Regression Graded assignments #####
getwd()
library(gains)
library(dplyr)
library(irr)
library(caret)
library(usmap)
path = 'C:\\Users\\SOMESH\\Downloads\\Compressed\\Snacks-Manufacturer-3eb4a1e972f8a66bd3ba4e0d82ae5a71dbbda253\\'
fileName = 'goodforu-class12.csv'
table<-read.csv(paste0(path,fileName), header = TRUE, stringsAsFactors = FALSE)

# From the survey, How many people believe that the Brand A is manu-
# factured with Farm-ingredients? 1 - Yes, 2 - No
table(table$X2)->a
a
# From the survey, How many people believe that the Brand A is manu-
# factured with natural oils? 1 - Yes, 2 - No
table(table$X16)->b
b
# From the survey, How many people believe that the Brand A is manu-
# factured with Zero-grams trans-fat? 1 - Yes, 2 - No
table(table$X9)->c
c

# The variable denoting the overall perception of Brand A is named as ?

# Answer : X23

# 50% of the people in survey have rated the overall perception of Brand A is as 4 or less

# Answer
library(dplyr)
table%>%filter(X23 <= 4)%>%count()/nrow(table)

# How many people have rated Brand A with >=5? Find out the percentage.
table%>%filter(X23 >= 5)%>%count()

# Are the following independent variable influence the overall perception of Brand A
# 1. Minimally processed items
# 2. Zero trans fat
# 3. Natural oils
# 4. Farm grown items
# Can we build the logistic regression with 4 independent variable?
# Anwer : Yes


# Log odds of having a good perception are more if someone said "Yes"
# to a question regarding the use of farm ingredients.

# Answer : Odds(odd of success) is directly proportional to Log odds. So, Yes

# If someone said "No" to a question regarding the use of "Natural Oils"
# then log odds of that person having a good perception about Brand A will
# decreased by___?

# Answer
table(table$X16)

round(log(0.6837936/0.5585552), 4)

# Is the level of processing is related to the overall perception of Brand A
# , statistically significant?

# Answer : Yes, Statitically significant

# Match the following
table(table$X23)

# Answer : No Idea