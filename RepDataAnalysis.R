library(dplyr)
library(data.table)
library(ggplot2)

download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", 
              destfile = "./cloudfront.zip")


unzip("./cloudfront.zip")

Data<-read.csv("./activity.csv", na.string = "NA")
print(str(Data))

CData<-aggregate(formula = steps~date,data = Data,FUN = sum)
hist(CData$steps,col ="orange", xlab="Steps",main ="Steps per day")

print(summary(CData))


CData2<-aggregate(steps~interval,Data,mean)
plot(steps~interval,data = CData2,type="l",ylab="Average Step",xlab="5-min interval",
     main = "Average Daily Activity Pattern")

print(CData2[which.max(CData2$steps),1])



print(sum(is.na(Data$steps)))
CData3<-Data
CData3[is.na(CData3$steps),1]<-0
CData3<-aggregate(steps~date,CData3,sum)
hist(CData3$steps,col ="gold", xlab="Steps",main ="Steps per day")

print(summary(CData3))


CData4<-Data
CData4[is.na(CData4$steps),1]<-0
CData4$date<-as.POSIXct(CData4$date, format = "%Y-%m-%d" )
CData4$Day<-weekdays(CData4[,2])
CData4$DayType<-as.factor(ifelse(CData4[,4]=="Sunday"| CData4[,4]=="Saturday"
                                 ,"Weekend","Weekday"))
WEData<-subset(CData4, DayType =="Weekend")
WDData<-subset(CData4, DayType == "Weekday")

WEData<-aggregate(steps~interval,WEData,mean)%>%mutate(DayType = "Weekend")
WDData<-aggregate(steps~interval,WDData,mean)%>%mutate(DayType ="Weekday")
CData4<-rbind(WEData,WDData)

graph<-ggplot(data = CData4, aes(x=interval,y=steps,color=DayType))
graph<-graph+geom_line()+facet_grid(DayType~.)+ylab("Average Steps")
graph<-graph+xlab("5-min Interval Time")
graph<-graph+ggtitle("Activity Patterns between Weekday and Weekend")
print(graph)


