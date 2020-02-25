
#### R For Data Science II ####

#Below code to import the contents of the dataset
library(XML)
setwd("C:\\Users\\Documents\\CASE STUDY\\1.R For Data Science\\Smart Certification Case Study\\dataset\\The World's Most Valuable Brands List - Forbes (1).html_ R for Data Science.._files")
u = c("The World's Most Valuable Brands List -.html")
tables = readHTMLTable(u)
tables$the_list
data <- tables$the_list
data
###################################################

#Q1: 1.(a) Use readHTMLTable() function from XML library in R to read this html file inside R. If you use class() function on 
#this object, the output is ____________
#(Hint: Just mention output, without any console details, make sure you do include quotation marks, if your output shows quotes)

#Ans1: 1.(a) "list"
class(tables)

#Q2: 1.(b) 1. (b) How many elements are inside the object you just read in?

#Ans2: 1.(b) 26.0000
length(tables)

#Q3: 1.(c)When you convert the relevant html table into a dataframe, how many columns do you obtain for this dataframe

#Ans3: 1.(c) 8.0000

length(data)

#Q4: 2.Now that you have the relevant data in the form of a dataframe, answer the following questions, keeping in mind you have the 
#correct dataframe with relevant data
#(a) In this dataframe, is there a column called "Brand_Revenue" ?

#Ans4: 2.(a) False

names(data)

#Q5: 2.(b) The data type of "Brand Value", column of this dataframe is numeric.

#Ans5: 2.(b) False

class(data$`Brand Value`)

#Q6: 2.(c) How many unique values are there in the column "Industry"?

#Ans6: 2.(c) 20.0000

length(unique(data$Industry))

#Q7: 2.(d) How many rows in the data for the column Industry, has a value "Automotive"?

#Ans7: 2.(d) 13.0000

class(data$Industry)
data$Industry<-as.character(data$Industry)
names(data)
names(data)[1]<-"Missing"
library(dplyr)
data%>%filter(Industry=="Automotive")%>%nrow()

#Q8: 3. Once you explore the dataframe, containing, relevant information, its time, that you start cleaning this dataframe. 
#Keeping this in mind, answer the following questions

#(a) For the column, called "Company Advertising" you can see that some observations have units in Millions of dollars, while 
#some are recorded in Billions of dollars. Count how many observations in this column are recorded in Millions of dollars.

#Ans8: 3.(a)23.0000

class(data$`Company Advertising`)
data$`Company Advertising`<-as.character(data$`Company Advertising`)
index=which(grepl("M",data$`Company Advertising`))
index

length(index)
    #OR
dat1<-data[index,]
nrow(dat1)

#Q9: 3. (b) In this table, there are many rows, across many columns, where, the values are missing, but aren't being treated as
#NA values. Take an appropriate action to resolve this issue., and fill in the blanks:
#Number of NA values in Company Advertising column is__________

#Ans9: 3.(b)38.0000

#Imputing Missing values in Brand Revenue column
class(data$`Brand Revenue`)
data$`Brand Revenue`<-as.character(data$`Brand Revenue`)
Na_indexes=grepl("-",data$`Brand Revenue`)
Na_indexes
data$`Brand Revenue`<-gsub("-","NA",data$`Brand Revenue`)
Na_indexes2=which(grepl("NA",data$`Brand Revenue`))
Na_indexes2
length(Na_indexes2)

#Imputing Missing Values in Company Advertising column and finding number of Na Values
data$`Company Advertising`<-gsub("-","NA",data$`Company Advertising`)
class(data$`Company Advertising`)
Na_index=which(grepl("NA",data$`Company Advertising`))
length(Na_index)

#Q10: 3. (c) After dropping the rows with missing values (be careful, there are some columns in your data with missing values 
#across all, rows, drop these columns, before you drop rows with missing values), we end up with............ rows in the dataframe

#Ans10: 3.(c) 60.0000

library(dplyr)
data%>%filter(`Company Advertising`!='NA')%>%as.data.frame()->data
data%>%filter(`Brand Revenue`!='NA')%>%as.data.frame()->data
nrow(data)

#Q11: 3. (d) Now, coming back to the column, talking about company advertisement, there are some rows in the data where the 
#observations are made in the units of millions of dollars. You need to normalize this data and make sure all the rows in this 
#column are measuring data in billions of dollars. After you normalize and clean this column, the average value of this column is 
#(round your answer to 2 decimal places using round() function)?

#Ans11: 3.(d)2.1100

class(data$`Company Advertising`)
index=which(grepl("M",data$`Company Advertising`))
index

data$`Company Advertising`<-gsub("M","",data$`Company Advertising`)
data$`Company Advertising`<-gsub("B","",data$`Company Advertising`)
data$`Company Advertising`<-gsub("\\$","",data$`Company Advertising`)

data$`Company Advertising`<-as.numeric(data$`Company Advertising`)

data$`Company Advertising`[index]<-data$`Company Advertising`[index]/1000

#Finding average value of Company Advertising
avg_value=mean(data$`Company Advertising`)
avg_value
round(avg_value,digits = 2)

#Q12. 3. (e) Now, clean the column Rank, Brand Value and Brand Revenue. What is the average Brand Value rounded to 2 decimal places?

#Ans12 3.(e) 20.7900

#Cleaning Rank column
class(data$Rank)
data$Rank<-gsub("#","",data$Rank)
data$Rank<-as.integer(data$Rank)

#Cleaning Brand Revenue column
class(data$`Brand Revenue`)
data$`Brand Revenue`<-gsub("B","",data$`Brand Revenue`)
data$`Brand Revenue`<-gsub("\\$","",data$`Brand Revenue`)

data$`Brand Revenue`<-as.numeric(data$`Brand Revenue`)

#Cleaning Brand Value column
class(data$`Brand Value`)
data$`Brand Value`<-gsub("B","",data$`Brand Value`)
data$`Brand Value`<-gsub("\\$","",data$`Brand Value`)

data$`Brand Value`<-as.numeric(data$`Brand Value`)

#Finding average Brand value
avg_value=mean(data$`Brand Value`)
round(avg_value,digits = 2)


save.image("CaseStudy-R-Solutions.RData")
######################################################################################################################################################