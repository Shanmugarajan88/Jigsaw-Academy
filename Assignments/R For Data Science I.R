
#### R For Data Science I####

#Q1: How would the 5th row and all columns in the given dataset "airquality" be chosen? 
#  Assume the following sequence of R codes is executed
#Data<-airquality

#Answer 1:
Data<-airquality
View(Data)
Data<-Data[5,]
Data

#Q2: How would the Column "Wind" in the dataset "airquality", which is an inbuilt dataset in R,be chosen?
#Data<-airquality

#Answer 2:
Data<-airquality
Data<-Data[,"Wind"]
View(Data)

#Q3: How will you display the first 10 odd numbers? 

#Answer 3:
K<-1:10;for(I in K){print(2*I-1)}

# Q4: Observe the following sequence of commands: 
# > a<-1:10
# > b<-1:10
# > c<-1:8
# If now we run the following command:
# > dim(data.frame(a,b,c))
# Why would we get an error?

#Answer 4 : You can't create a data by combining vectors of different lengths 
# a<-1:10
# b<-1:10
# c<-1:8
# dim(data.frame(a,b,c))

# Q5: Refer to the sequence of R commands:
# > rep(seq(1:2),3)
# What is the output that will be displayed?
  
#Answer 5:
rep(seq(1:2),3)

# Q6: Observe the following sequence of commands:
# > a <- 1:10
# > b <- 1:10
# > c <- 1:8
# If now we run the following command:
# > dim(data.frame(a,b,c))
# What would happen?

#Answer 6:An error message will be displayed 

# a<-1:10
# b<-1:10
# c<-1:8
# dim(data.frame(a,b,c))

# Q7 :We have the following code:
#   
# >x<-c(500,1500,NA,NA,2500)
# >Mean_x <- mean(na.omit (x))
# >Mean_x
# What will be the output?

#Answer 7:
x<-c(500,1500,NA,NA,2500)
Mean_x<-mean(na.omit(x))
Mean_x


save.image("RForDataScience.RData")
##################################################################################################h
























