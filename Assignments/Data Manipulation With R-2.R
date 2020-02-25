
#### Data Manipulation With R-2 ####

#Question 1 :

#Read the file MMwoes.csv. What is the first quartile value for loan_amount?

#Answer 1:
mw<-read.csv("MMwoes.csv")
View(mw)
str(mw)
mw$X.2<-as.numeric(as.character(mw$X.2))
quantile(mw$X.2,na.rm = TRUE)

#Question 2 :

#In the file MMwoes.csv, what is the data type of column named next_payment_d, once it is read in a 
#dataframe?

#Answer 2 :
str(mw)
View(mw)

#Question 3 :

#Imagine you have a dataframe, called big_data. Suppose you want to save this as a .csv file 
#to "C:\\My Documents\\Project1". The name of this file should be "big_Data.csv". 
#Another requirement is that, we don't want the row numbers to be populated when the data is exported. 
#Choose the code snippet that will help in achieving this?

#Answer 3 : write.csv(big_data, "C:\\My Documents\\Project1\\big_Data.csv",row.names=F)

#Question 4 :

#Use the files named candidates.csv and contributions.csv. The file candidates.csv contains data about 
#the demographics and party affiliation of donors to different political parties. 
#The file contributions.csv contains data relating to details of donations made. 
#What is the average amount of donations given to Republicans? 
#(In the party column R stands for Republicans and D stands for Democrats)

#Answer 4 :
cand<-read.csv("candidates.csv")
contr<-read.csv("contributions.csv")

View(cand)
View(contr)

class(cand$first_name)
class(cand$last_name)
class(cand$middle_name)

class(contr$first_name)
class(contr$last_name)
class(contr$middle_name)

cand$first_name<-as.character(cand$first_name)
cand$last_name<-as.character(cand$last_name)
cand$middle_name<-as.character(cand$middle_name)


contr$first_name<-as.character(contr$first_name)
contr$last_name<-as.character(contr$last_name)
contr$middle_name<-as.character(contr$middle_name)

cand$first_name<-paste(cand$first_name,cand$middle_name,cand$last_name,sep = " ")

cand$first_name

cand$last_name<-NULL
cand$middle_name<-NULL
names(cand)

contr$first_name<-paste(contr$first_name,contr$middle_name,contr$last_name,sep = " ")

contr$first_name

contr$last_name<-NULL
contr$middle_name<-NULL
names(contr)

names(cand)[1]<-"candidate_id"
result<-merge(x=cand,y=contr,by="candidate_id")
View(result)
library(dplyr)
result%>%filter(party=="R")%>%summarize(mean(amount))

#Question 5 :

#Use the files named candidates.csv and contributions.csv. The file candidates.csv contains data about the demographics and party 
#affiliation of donors to different political parties. The file contributions.csv contains data relating to details of donations made. 
#What was the highest amount of contribution made to democrats? (In the party column R stands for Republicans and D stands for Democrats)

#Answer 5 :
names(cand)
names(cand)[1]<-"candidate_id"
result<-merge(x=cand,y=contr,by="candidate_id")
View(result)
library(dplyr)
result%>%filter(party=="D")%>%summarize(max(amount))

#Question 6 :

#Use the files named candidates.csv and contributions.csv. The file candidates.csv contains data about the demographics and 
#party affiliation of donors to different political parties. The file contributions.csv contains data relating to details of donations made.
#What was the least amount of contribution made to democrats? (In the party column R stands for Republicans and D stands for Democrats)

#Answer 6 :
names(cand)[1]<-"candidate_id"
result<-merge(x=cand,y=contr,by="candidate_id")
View(result)
library(dplyr)
result%>%filter(party=="D")%>%summarize(min(amount))

#Question 7 :

#Use the files named candidates.csv and contributions.csv. The file candidates.csv contains data about the demographics and party 
#affiliation of donors to different political parties. The file contributions.csv contains data relating to details of donations made. 
#Which state brings highest average contributions to Democrats? (In the party column R stands for Republicans and D stands for Democrats)

#Answer 7 :
names(cand)[1]<-"candidate_id"
result<-merge(x=cand,y=contr,by="candidate_id")
View(result)
library(dplyr)
result%>%group_by(state)%>%filter(party=="D")%>%summarize(mean(amount))

#Question 8 :

#Use the files named candidates.csv and contributions.csv. The file candidates.csv contains data about the demographics and party 
#affiliation of donors to different political parties. The file contributions.csv contains data relating to details of donations made. 
#Which state brings highest average contributions to Republicans? (In the party column R stands for Republicans and D stands for Democrats)

# Answer 8 :
names(cand)[1]<-"candidate_id"
result<-merge(x=cand,y=contr,by="candidate_id")
View(result)
library(dplyr)
result%>%group_by(state)%>%filter(party=="R")%>%summarize(mean(amount))%>%as.data.frame()->dataop
class(dataop)
View(dataop)

###############################################END OF CASE STUDY GRADED ASSIGNMENT##########################################################

save.image("CaseStudyGraded-DM.RData")
