
#### Data Preparation ####

#A merge between "Transactions_File.txt" and "Products_File.txt" files is required to answer
#below questions based on the transactions,store as "tr_product" in R.

trans<-read.table("Transactions_File.txt",header = T,sep="\t")
product<-read.table("Products_File.txt",header = T,sep="\t")

tr_product=merge(x=trans,y=product,by="Product_Code")

#Ques 1. 1. a) Which product category dominates in terms of total purchase amount in dollars?

#Ans 1. 1. a) Entertainment

library(dplyr)

tr_product%>%group_by(Product_Category)%>%summarise(Total_Purchase=sum(Items_Amount))%>%arrange(-Total_Purchase)%>%as.data.frame()->pr_category
View(pr_category)

#Ques 2. 1. b) Which product category has lowest contribution in total purchase amount in dollars?
#Ans 2. 1. b) Software

tr_product%>%group_by(Product_Category)%>%summarise(Total_Purchase=sum(Items_Amount))%>%arrange(Total_Purchase)%>%as.data.frame()->pr_category
View(pr_category)

#Ques 3. 1. c) Which mode of payment is most popular?
#Ans 3. 1. c) CreditCard

tr_product%>%group_by(Payment_Method)%>%summarise(Count=n())%>%arrange(-Count)%>%as.data.frame()->payment_count
View(payment_count)

#Ques 4. 1. d) How many transactions were carried out during 18:00 hour?
#Ans 4. 1. d) 798

class(tr_product$Timestamp)

library(lubridate)

tr_product$Timestamp<-as.character(tr_product$Timestamp)

tr_product$Timestamp<-ymd_hms(tr_product$Timestamp)

tr_product%>%filter(hour(Timestamp)==18)%>%summarise(Count=n())

#Ques 5. 1. e) Is Mode of Payment affected by time of transactions?
#(Hint: Create a contingency table between "Hour" and "Payment_Method" . Use chisq.test() )
#Ans 5. 1. e) No

library(lubridate)

tr_product$Timestamp<-as.character(tr_product$Timestamp)

tr_product$Timestamp<-ymd_hms(tr_product$Timestamp)

tr_product$Hour<-hour(tr_product$Timestamp)

#Null hypothesis : Mode of payment is not affected by time of transaction
#Alt hypothesis : Mode of payment is affected by time of transaction

chisq.test(tr_product$Hour,tr_product$Payment_Method)

#p-value after chisq.test() comes out to be 0.6783 and alpha value is 0.05.
#Because p-value > alpha-value therefore Accept Null hypothesis .
#Therefore, mode of payment is not affected by time of transaction


#Ques 6. A merge between "Customers_File.txt" and "Transactions_File.txt" files is required to answer
#below questions based on the transactions, store as "tr_crust" in R.
# 2) a) How many records will be there in "tr_cust" object after removing the duplicate rows by "Card_ID"?
#Ans 6. 2) a) 59477

cust<-read.table("Customers_File.txt",header = T,sep="\t")
tran<-read.table("Transactions_File.txt",header = T,sep="\t")

tr_cust<-merge(x=cust,y=tran,by="Card_ID")

#Remove duplicate rows of the dataframe by Card_ID variable 
#The .keep_all function is used to retain all other variables in the output data frame.
tr_cust%>%distinct(Card_ID,.keep_all = TRUE)%>%summarise(No_of_Records=n()) 

#Ques 7. 2) b) Find Age of customers as on '1-Jan-2017' using "Birth_Date" variable then perform a 
#suitable age grouping and analyse which group has maximum contribution in terms of amount spent in dollars.
#(Hint : Group customers age in the sequence 'seq(25,115,by=15)')
#Ans 7. 2) b) (40,55]

cust<-read.table("Customers_File.txt",header = T,sep="\t")
tran<-read.table("Transactions_File.txt",header = T,sep="\t")

tr_cust<-merge(x=cust,y=tran,by="Card_ID")

library(lubridate)

tr_cust$Birth_Date<-ymd(tr_cust$Birth_Date)

#Finding Age of customers 

dob<-tr_cust$Birth_Date

current_date="2017-01-01"

year=duration(num=1,units="years")

tr_cust<-mutate(tr_cust,Age=interval(dob,current_date)/year)

tr_cust$Age<-ceiling(tr_cust$Age)

#Performing Age grouping

tr_cust$Age_Group<-cut(tr_cust$Age,breaks=c(25,40,55,70,85,100,115),labels = c("25-40","40-55","55-70","70-85","85-100","100-115"))

#Finding which group has maximum contribution 

tr_cust%>%group_by(Age_Group)%>%summarise(Total_Contribution=sum(Items_Amount))%>%arrange(-Total_Contribution)->contr

View(contr)

#Ques 8. 2) c) Based on Age and Gender, which group of customers dominate amount spent in dollars?
#(Hint: Use Age groups as created in 2b)
#Ans 8. 2) c) (40,55],M

tr_cust%>%group_by(Age_Group,Gender)%>%summarise(Total_Contribution=sum(Items_Amount))%>%arrange(-Total_Contribution)->cont_age_gender

View(cont_age_gender)

#Ques 9. A merge between "Customers_File.txt" and "Campaign_File.txt" files is required to answer below questions 
#based on the campaign responses, store as "cc" in R.
# 3) a) What is the proportion of customers who responded(TRUE) to the campaign, rounded off to three decimal points, 
# use the round() in R ? 
#Ans 9. 3) a) 0.055

custr<-read.table("Customers_File.txt",header = T,sep="\t")
campg<-read.table("Campaign_File.txt",header = T,sep="\t")

cc<-merge(x=custr,y=campg,by="Card_ID")

cc%>%filter(Campaign_Responce=="TRUE")->ResponseTrue

nrow(ResponseTrue)
nrow(cc)

proportion_responded_true_to_campaign=nrow(ResponseTrue)/nrow(cc)
proportion_responded_true_to_campaign

round(proportion_responded_true_to_campaign,digits = 3)

#Ques 10. 3) b) Identify the 'Tenure' group of customers where response rate is high.
#Tenure will be defined as the time period between the Date of Registration and 31/12/2004.
#(Hint: Group Tenure of customers in the sequence 'seq(3,7.5,by=1.5)')
#Ans 10. 3) b) (4.5-6.0]

cc$Registration_Date<-ymd(cc$Registration_Date)

#Finding Tenure 

date_r<-cc$Registration_Date

current_date1<-"2004-12-31"

year=duration(num=1,units = "years")

cc<-mutate(cc,Tenure=interval(date_r,current_date1)/year)

#Performing Tenure grouping of customers

cc$Tenure_Group<-cut(cc$Tenure,breaks=c(3.0,4.5,6.0,7.5),labels=c("3.0-4.5","4.5-6.0","6.0-7.5"))

#Finding which group has high response rate

cc%>%group_by(Tenure_Group)%>%summarise(Response_rate=sum(Campaign_Responce,na.rm = TRUE))%>%arrange(-Response_rate)->tenure_rr

#Ques 11. 3) c) How many customers are there with tenure between 6 and 7.5?
#Ans 11. 3) c)1836

cc%>%filter(Tenure_Group=='6.0-7.5')%>%as.data.frame()->cc1
nrow(cc1)
