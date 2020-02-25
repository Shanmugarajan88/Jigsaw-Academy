
#### Data Manipulation With R-3 ####

#Question1

#Use the inbult iris dataset, what is mean Sepal.Length for species setosa?

library(dplyr)

#Answer1 :
iris%>%filter(Species=='setosa')%>%summarize(mean(Sepal.Length))

#Question2

#Use the inbuilt mtcars data set, what is the average "mpg" for each category of car defined based 
#on "number of gears".

# Answer2 :
data(mtcars)
m<-mtcars
library(dplyr)
m%>%group_by(gear)%>%summarize(mean(mpg))

#Question3:

#Load the inbuilt dataset AdultUCI from arules package. How many females are there in the data set 
#whose age is less than 50 and who are black?

#Answer 3 :
library(arules)
data(AdultUCI)
ad<-AdultUCI
library(dplyr)
ad%>%filter(age<50,race=='Black',sex=='Female')%>%nrow()

#Question4

#Use the lakers data set in the package lubridate. How many times observations corresponding to the 
#months of October, December, January and April, appear?

#Answer 4 :
library(lubridate)
data(lakers)
lake<-lakers
library(dplyr)
lake$Date<-paste(as.character(lake$date),lake$time)
lake$Date<-ymd_hm(lake$Date)

lake%>%group_by(Month=months(Date))%>%summarize(Number=n())%>%filter(Month=='October' | Month=='December' | Month=='January' | Month=='April')

#Question5

#Use the lakers data set in the package lubridate. In this data set how many rows correspond to 
#instances where the Player was Pau Gasol and the opposition was POR?


#Answer 5:
library(lubridate)
library(dplyr)
data(lakers)
lake<-lakers
lake$Date<-paste(as.character(lake$date),lake$time)
lake$Date<-ymd_hm(lake$Date)
lake%>%filter(player=='Pau Gasol',opponent=='POR')%>%nrow()

#Question 6:

#Use the lakers data set in the package lubridate. How many rows correspond to a scenario, when 
#opponent to POR was LAL?
  
#Answer 6:
library(lubridate)
library(dplyr)
data(lakers)
lake<-lakers
lake$Date<-paste(as.character(lake$date),lake$time)
lake$Date<-ymd_hm(lake$Date)
lake%>%filter(opponent=='POR',team=='LAL')%>%nrow()

#Question7

#Use the AdultUCI data set from the arules() package. How many 38 year olds are divorced?

#Answer 7:
library(arules)
library(dplyr)
data(AdultUCI)
adu<-AdultUCI
names(adu)
##Ask   -----adu<-filter(age==38,`marital-status`=='Divorced')%>%nrow()
adu%>%filter(`marital-status`=="Divorced",age==38)%>%nrow()

#Question8

#Use the lakers data set in the package lubridate. In this data how many rows correspond to 
#instances where the day was Monday and time 12?

#Answer 8:
library(lubridate)
library(dplyr)
data(lakers)
lake<-lakers
lake$Date<-paste(as.character(lake$date),lake$time)
lake$Date<-ymd_hm(lake$Date)
lake%>%filter(weekdays(Date)=='Monday',hour(Date)==12)%>%nrow()

#Question9

#Use the lakers data set in the package lubridate. In this data how many rows correspond to instances 
#where, the match was played on Home ground, the opponent wsa PHX and day was Wednesday?

#Answer 9:
library(lubridate)
library(dplyr)
data(lakers)
lake<-lakers
lake$Date<-paste(as.character(lake$date),lake$time)
lake$Date<-ymd_hm(lake$Date)
lake%>%filter(game_type=='home',opponent=='PHX',weekdays(Date)=='Wednesday')%>%nrow()

#Question10

#Using the data set AdultUCI from the package arules() find out the mean age corresponding to all 
#categories in the column "income" for all white females who work less than 25 hours a week. 
#(Consider the missing values in the column "income" also as a separate group)

#Answer 10:
library(arules)
library(dplyr)
data(AdultUCI)
adu<-AdultUCI
adu%>%group_by(Category=income)%>%filter(`hours-per-week`<25,sex=="Female",race=="White")%>%summarize(mean(age))

save.image("NonGradedQuiz-DM.RData")
