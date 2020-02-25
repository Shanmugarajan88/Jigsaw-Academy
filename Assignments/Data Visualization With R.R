
#### DATA VISUALIZATION WITH R ####

#The dataset used here is about the valuation of different brands, the advertising expenditure on them, the
#global revenue, etc. You need to use grammar of graphics based visualization framework, to create
#graphics like the ones shown below for each of the following sectors:
#(a) Technology (b) Luxury (c) Automotive (d) Financial Services

#CREATING GRAPHICS FOR TECHNOLOGY

data=read.csv("dataF.csv")

library(ggplot2)
library(dplyr)

data%>%filter(Industry=="Technology")->data1
p<-ggplot(data1,aes(x=Company.Advertising,y=Brand.Revenue,size=Brand.Value,color=Brand))
q<-p+geom_point()
q+xlab("Company Advertising in Billions of $")+ylab("Brand Revenue in Billions of $")+scale_size(range = c(2,4),breaks=c(30,60,100),name="Brand Value $ (Billions)")+geom_text(aes(label=Brand),hjust=0.5,vjust=1)+guides(color=FALSE)+theme_light()+theme(legend.key=element_rect(fill="light blue",colour="black"))


#CREATING GRAPHICS FOR LUXURY

data%>%filter(Industry=="Luxury")->data2
p<-ggplot(data2,aes(x=Company.Advertising,y=Brand.Revenue,size=Brand.Value,color=Brand))
q<-p+geom_point()
q+xlab("Company Advertising in Billions of $")+ylab("Brand Revenue in Billions of $")+scale_size(range=c(2,4),breaks=c(10.0,28.1),name="Brand Value $ (Billions)")+geom_text(aes(label=Brand),hjust=0.7,vjust=1.7)+guides(color=FALSE)+theme_light()+theme(legend.key = element_rect(fill="light blue",colour="black"))+scale_x_continuous(breaks = seq(0,6,0.1))



#CREATING GRAPHICS FOR FINANCIAL
data%>%filter(Industry=="Financial Services")->data3
p<-ggplot(data3,aes(x=Company.Advertising,y=Brand.Revenue,size=Brand.Value,color=Brand))
q<-p+geom_point()
q+xlab("Company Advertising in Billions of $")+ylab("Brand Revenue in Billions of $")+scale_size(range=c(2,4),breaks=c(7.0,12.0,23.4),name="Brand Value $ (Billions)")+geom_text(aes(label=Brand),hjust=0.7,vjust=1.7)+guides(color=FALSE)+theme_light()+theme(legend.key = element_rect(fill="light blue",colour="black"))+scale_x_continuous(breaks=seq(0.6,4,0.1))+scale_y_continuous(breaks = seq(10,100,10))


#CREATING GRAPHICS FOR AUTOMOTIVE
data%>%filter(Industry=="Automotive")->data4
p<-ggplot(data4,aes(x=Company.Advertising,y=Brand.Revenue,size=Brand.Value,color=Brand))
q<-p+geom_point()
q+xlab("Company Advertising in Billions of $")+ylab("Brand Revenue in Billions of $")+scale_size(range=c(2,4),breaks=c(6.2,20.0,37.8),name="Brand Value $ (Billions)")+geom_text(aes(label=Brand),hjust=0.7,vjust=1.7)+guides(color=FALSE)+theme_light()+theme(legend.key=element_rect(fill="light blue",colour="black"))+scale_x_continuous(breaks = seq(0.8,6,0.1))+scale_y_continuous(breaks = seq(40,200,10))


save.image("CaseStudy-DataVisualization-Assign.RData")
