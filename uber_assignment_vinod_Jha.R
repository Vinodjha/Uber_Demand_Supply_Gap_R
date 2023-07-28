#Run the ggplot() and ggarrange() without running png() command prior to them to see
#on the R window

#loading required libraries
library(ggplot2)
library(ggpubr)
library(stringr)
library(tidyr)
library(dplyr)


setwd("C:/Users/user/Desktop/DA/UBER case study")
uber<-read.csv("Uber Request Data.csv",stringsAsFactors = F)
uber<-unique(uber)# removing duplicate rows, if any
str(uber)

#some dates were having "/" as separator, changing separator to "-"
uber$Request.timestamp<-str_replace_all(uber$Request.timestamp,"/","-")
uber$Drop.timestamp<-str_replace_all(uber$Drop.timestamp,"/","-")

#SOme dates were having month in single digit, that will be dealt by POSIXct while converting to standard date-time format
#seconds are not needed for this analysis.Also, some time entries had missing second values, it does not hamper the analysis
#So, second values are assumed to zero for all the entries
uber$Request.timestamp<-as.POSIXct(uber$Request.timestamp,format="%d-%m-%Y %H:%M")
uber$Drop.timestamp<-as.POSIXct(uber$Drop.timestamp,format="%d-%m-%Y %H:%M")

#new variables namely "request_hour" and "request_date" are derived from Request.timestamp column
uber$request_hour<-format(uber$Request.timestamp,"%H")
uber$request_date<-format(uber$Request.timestamp,"%d")


# plotting and saving of number of cab requests on individual days against hours
# geom_bar() is used for plotting as it is suitable to have a frequency plot of request on hourly basis
#aesthetic fill=factor(Status) does the job of filling the bars with different colours based on proportion of different statuses
plot11<-ggplot(filter(uber,request_date==11),aes(x=request_hour,fill=factor(Status)))+geom_bar(position="dodge")+ggtitle("Hourwise frequency of \n requests on 11th JUly")+labs(y="Number of requests")
plot12<-ggplot(filter(uber,request_date==12),aes(x=request_hour,fill=factor(Status)))+geom_bar(position="dodge")+ggtitle("Hourwise frequency of \n requests on 12th July")+labs(y="Number of requests")
plot13<-ggplot(filter(uber,request_date==13),aes(x=request_hour,fill=factor(Status)))+geom_bar(position="dodge")+ggtitle("Hourwise frequency of \n requests on 13th July")+labs(y="Number of requests")
plot14<-ggplot(filter(uber,request_date==14),aes(x=request_hour,fill=factor(Status)))+geom_bar(position="dodge")+ggtitle("Hourwise frequency of \n requests on 14th July")+labs(y="Number of requests")
plot15<-ggplot(filter(uber,request_date==15),aes(x=request_hour,fill=factor(Status)))+geom_bar(position="dodge")+ggtitle("Hourwise frequency of \n requests on 15th July")+labs(y="Number of requests")
plot16<-ggplot(uber,aes(x=request_hour,fill=factor(Status)))+geom_bar(position="dodge")+ggtitle("overall Hourwise frequency of requests")

png('comparision_request.png', width=1024, height=512)
ggarrange(plot11,plot12,plot13,plot14,plot15,plot16,nrow=2,ncol=3,labels = c("A","B","C","D","E","F"))
dev.off()
#Approximation: above plots shows similar trend of cab requests on all the working days. SO te reduce time and analysis,
#number of cab requests every hour are added for the all the days(days are no longer taken into analysis)

#plotting and saving total number of cab requests over five days against hours 

png('hourly_req_overall.png',height=512,width=512)
ggplot(uber,aes(x=request_hour,fill=Status))+geom_bar()+ggtitle("overall Hourwise frequency of requests")+labs(y="Number of requests")
dev.off()

#two new dfs are created for seperate analysis of "cancelled requests" and " no cars available cases" 
req_cancel<-filter(uber,uber$Status=="Cancelled")
req_nocars<-filter(uber,uber$Status=="No Cars Available")

#plotting and saving the number of cab requests for which either "cars are not available" or "requests are cancelled"
plot1<-ggplot(req_cancel,aes(x=request_hour,fill=factor(Pickup.point)))+geom_bar(position="dodge")+ggtitle("Hourwise frequency of cancelled request")+labs(y="Number of cancelled requests")
plot2<-ggplot(req_nocars,aes(x=request_hour,fill=factor(Pickup.point)))+geom_bar(position = "dodge")+ggtitle("Hourwise frequency of events \n when no cars where available")+labs(y="Number of requests when cars not available")

png('cancelled_request.png',width=512,height=512)
print(plot1)
dev.off()

png('nocars_request.png',width=512,height=512)
print(plot2)
dev.off()

# creation of a df with hourly supply and demand cab numbers(segmented analysis: request_hour is the basis of segmentation)
hours_summary<-group_by(uber,request_hour,Status,Pickup.point)
hours_summary<-as.data.frame(summarise(hours_summary,count=n()))
hours_summary<-spread(hours_summary,Status,count)
hours_summary$Cancelled[which(is.na(hours_summary$Cancelled)==T)]=0 # replacing all The NA values with zero for additions
hours_summary$gap<-hours_summary[,4]+hours_summary[,3]  # creating a new variable called gap, which indicates the gap between demand and supply
hours_summary$total_requests<-hours_summary$gap+hours_summary$`Trip Completed` # total number of requests(demand) in a particular hour
hours_summary1<-hours_summary  # complte info about all the request status and pick point
hours_summary<-hours_summary[,c(-4,-3)]
colnames(hours_summary)<-c("request_hour","Pickup.point","supply","gap","demand")
hours_summary #hourwise gap between supply and demand (only supply and gaps related column are retained)
hours_summary$per_gap<-hours_summary$gap *100/hours_summary$demand # percentage gap between demand and supply(mutation)

#creation of a derived metric called time_of_day e.g. early morning,afternoon etc
#Early morning:4am to 8 am,Late morning: 8 am to 12pm,Afternoon:12 pm to 6 pm
#Early evening:6 to 9 pm,Late evening:9 pm to 11 pm,Night:11 pm to 4 am 
hours_summary$request_hour<-as.integer(hours_summary$request_hour)
hours_summary$time_of_day<- ifelse(hours_summary$request_hour<=4,"Night",ifelse(hours_summary$request_hour<=8,"Early Morning",
                            ifelse(hours_summary$request_hour<=12,"Late Morning",ifelse(hours_summary$request_hour<=18,"Afternoon",
                            ifelse(hours_summary$request_hour<=21,"Early Evening","Late Evening")))))
hours_summary$request_hour<-as.character(hours_summary$request_hour)


#plotting and saving the graph between gap of demand & supply and request_hour for different pickup.point
#geom_col() is used to plot here, as geom_bar is suitable to get a frequency plot where as
#geom_col() is suitable to plot one categorical variable vs another quantitave variable
plot3<-ggplot(hours_summary,aes(x=request_hour,y=gap))+geom_col( )+ggtitle("Hourwise gap between demand and supply")+labs(y="gap between demand and supply")
plot4<-ggplot(hours_summary,aes(x=request_hour,y=gap,fill=Pickup.point))+geom_col(position="dodge")+ggtitle("Hourwise gap between demand and \n supply for different pickup point")+labs(y="gap between demand and supply")
png('gap_hour.png',width=512,height = 512)
ggarrange(plot3,plot4, nrow=2,ncol=1,labels=c("A","B"))
dev.off()

# plot of (A)percentatge gap between demand and supply every hour, (B) Percentage gap vs time_of_day
plot5<-ggplot(hours_summary,aes(x=request_hour,y=per_gap,fill=Pickup.point))+geom_col(position="dodge")+ggtitle("Hourwise % gap between demand and \n supply for different pickup point")+labs(y="% gap between demand and supply")
plot6<-ggplot(hours_summary,aes(x=factor(time_of_day),y=per_gap,fill=Pickup.point))+geom_col(position="dodge")+ggtitle("% gap between demand and  supply vs time_of_day\n for different pickup point")+labs(y="% gap between demand and supply")
png('per_gap_hour.png',width=512,height = 512)
ggarrange(plot5,plot6, nrow=2,ncol=1,labels=c("A","B"))
dev.off()



# For all the comparisons and analysis, Bar graphs are chosen as Bar graphs claerly 
#show the count of occurences of a variable. It can also show the groupwise counts of a 
#variable based on another categorical variable
