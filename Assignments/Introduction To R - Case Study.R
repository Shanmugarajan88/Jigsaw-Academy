
#### Case Study: Introduction To R ####

#1.Create a data frame named customer_details, with 3 rows and 4 columns and following values:

customer_details<-data.frame(Name=c("Ramya","Ali","Jim"),Age=c(25,30,35),
                             Telephone_bill_rs=c(600,400,200),Month=c("Aug","Aug","Aug"))
customer_details

#2. This question is related to creating lists. Write code for following steps :
# a) Create a list called names, include following attributes in this list :
#  a. last_name: Potter, Riddle, Dumbledore
#  b. first_name: Harry, Tom, Albus
#  c. age: 18,50,120
#  d. profession: Student, Magician, Headmaster

names<-list(last_name=c("Potter","Riddle","Dumbledore"),first_name=c("Harry","Tom","Albus"),
            age=c(18,50,120),profession=c("Student","Magician","Headmaster"))
names

#b) How will you display all the values in attribute last_name?

names$last_name

#c) How will you display the 3rd element of attribute age?

names[["age"]][3]

#3. Write code for the following steps :
# a) Load the package ggplot2 into the workspace

library(ggplot2)

# b) Load the data msleep into the workspace. To know more about this dataset, run the command ?msleep

data("msleep")

?msleep

# c) Display all the names of this dataset

names(msleep)

# d) Rename the column vore to type

names(msleep)[3]<-"type"

# e) Display first ten values in the column type you just created

msleep[["type"]][1:10]

# f) Choose the columns name, genus, type, and sleep_total from the dataset and store it in a new
#dataset. Save the converted data set in your working directory using write.csv () function.

#setwd("C:\\Users\\NikSid\\Documents\\JIGSAW ACADEMY\\ASSIGNMENTS\\NON-GRADED\\CASE STUDIES\\Introduction-To-R")
attach(msleep)
msleep_new<-data.frame(name,genus,type,sleep_total)
write.csv(msleep_new,"msleep_new.csv",row.names = FALSE)

#4. This Question is to get you comfortable with reading excel workbook and worksheets.
#Follow the steps as mentioned below :
#  a) Load the package readxl to read excel files

library(readxl)

#Write a code to load the workbook retail.xlsx in the location C\DS Full stack\Assignments\Non Graded
#Assignment\ R for Data Science

read_excel("retail.xlsx")

#b) Read the two worksheets data1 and data2 separately into two data frames

df1=read_excel("retail.xlsx",sheet="data1")
df2=read_excel("retail.xlsx",sheet="data2")


save.image("CaseStudy-IntroToR-NonGraded.RData")
