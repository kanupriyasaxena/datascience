library(tidyr)
library(date)
library(ggplot2)

#read data from the file
uberdata<-read.csv("Uber Request Data.csv",stringsAsFactors=F,strip.white = TRUE)


#data cleaning started
summary(uberdata)
#removing NAs rows would remove a lot of cancelled and Cars Not Available Data, so do not remove NAs here

#Split Date Time into Date and Time
uberdata<-separate(uberdata, Request.timestamp, c("RequestDate", "RequestTime"), sep = " ", remove = TRUE)
uberdata<-separate(uberdata, Drop.timestamp, c("DropDate", "DropTime"), sep = " ", remove = TRUE)
uberdata<-separate(uberdata,DropTime,c("DHour","DMinute","DSecond"),sep = ":", remove = TRUE)
uberdata<-separate(uberdata,RequestTime,c("RHour","RMinute","RSecond"),sep = ":", remove = TRUE)

#Change the Hour columns to integer
uberdata$RHour<-as.integer(uberdata$RHour)
uberdata$DHour<-as.integer(uberdata$DHour)


#Format the Date Fieds
uberdata$RequestDate<-gsub("/","-",uberdata$RequestDate)
uberdata$RequestDate <- as.POSIXct(uberdata$RequestDate, format = "%d-%m-%Y")
uberdata$DropDate<-gsub("/","-",uberdata$DropDate)
uberdata$DropDate <- as.POSIXct(uberdata$DropDate, format = "%d-%m-%Y")

summary(uberdata)

#Plot1: "Number of Requests vs Pick Up stacked by Status"

ggplot(data=uberdata, aes(x=uberdata$Pickup.point),group=uberdata$Status) +
  geom_bar(stat="count",aes(fill=uberdata$Status))+
  labs(x="Pick Up", y="Number Of Requests", 
       title="Number of Requests vs Pick Up stacked by Status")



#subset data for airport pick up
uberdata_ap<-subset(uberdata,uberdata$Pickup.point=="Airport")

#Plot2: For Airport Pick Up:"Number of Requests vs Hour of Day stacked by Status"
ggplot(data=uberdata_ap, aes(x=uberdata_ap$RHour),group=uberdata_ap$Status) +
  geom_bar(stat="count",aes(fill=uberdata_ap$Status))+
  labs(x="Hour of Day", y="Number Of Requests", 
       title="Number of Requests vs Hour of Day stacked by Status")


#subset data for city pick up
uberdata_ct<-subset(uberdata,uberdata$Pickup.point=="City")

#Plot3: For City Pick Up:"Number of Requests vs Hour of Day stacked by Status"
ggplot(data=uberdata_ct, aes(x=uberdata_ct$RHour),group=uberdata_ct$Status) +
  geom_bar(stat="count",aes(fill=uberdata_ct$Status))+
  labs(x="Hour of Day", y="Number Of Requests", 
       title="Number of Requests vs Hour of Day stacked by Status")

