
#### Data Manipulation With R-1 ####

#1. Find out the number of delayed flights for all weekdays 

library(readxl)

fd<-read.csv("FlightDelays.csv")

library(dplyr)
library(lubridate)

fd$Date<-mdy(fd$date)

fd%>%filter(delay=="delayed"& weekdays(Date)!="Saturday"& weekdays(Date)!="Sunday")%>%nrow()

#Another way

obj1<-filter(fd,delay=="delayed")
obj2<-filter(obj1,weekdays(Date)!="Saturday" & weekdays(Date)!="Sunday")
nrow(obj2)

#2.Find the average distance, total distance and count for all delayed flights on Friday. 

library(dplyr)
library(lubridate)

fd$Date<-mdy(fd$date)

fd%>%filter(delay=="delayed",weekdays(Date)=="Friday")%>%summarize(Average=mean(distance),Total=sum(distance),Count=n())

#Another way

obj3<-filter(fd,weekdays(Date)=="Friday")
obj4<-filter(obj3,delay=="delayed")
nrow(obj4)
mean(obj4$distance)
sum(obj4$distance)

#3. Find out how many flights were on time on Week days and Weekends (Consider Saturday
#and Sunday as weekends) 

library(dplyr)
library(lubridate)

fd$Date<-mdy(fd$date)

#number of ontime flights on weekends
fd%>%filter(weekdays(Date)=="Saturday" | weekdays(Date)=="Sunday",delay=="ontime")%>%nrow()
#number of ontime flights on weekdays
fd%>%filter(weekdays(Date)!="Saturday" & weekdays(Date)!="Sunday",delay=="ontime")%>%nrow()


#4.  Find out the number of flights for each destination across all weekdays 

library(dplyr)
library(lubridate)

fd$Date<-mdy(fd$date)

fd%>%filter(weekdays(Date)!="Saturday" & weekdays(Date)!="Sunday")%>%group_by(dest)%>%summarize(n())

#5.  Find out the number of times weather was bad across all weekdays. (1 indicates bad
#weather)

library(dplyr)
library(lubridate)

fd$Date<-mdy(fd$date)

fd%>%filter(weekdays(Date)!="Saturday" & weekdays(Date)!="Sunday",weather==1)%>%nrow()

save.image("CaseStudy-NonGradedAssign.RData")
