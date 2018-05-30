install.packages("zoo")
library(zoo)
ukacc <- data.frame(Seatbelts, date = as.Date(as.yearmon((time(Seatbelts)))))

par(3,1)


              
  ggplot()+geom_line(data=ukacc,aes(x=ukacc$date,y=ukacc$front,col="red"))+
geom_line(data=ukacc,aes(x=ukacc$date,y=ukacc$drivers,col="blue"))+geom_line(data=ukacc,aes(x=ukacc$date,y=ukacc$rear,col="green"))
  #+layer(geom="line") 
   + geom_vline(data=ukacc,mapping=aes(xintercept=ukacc$date))
                
  ?geom_vline
    
  ggplot(data=ukacc,aes(x=ukacc$date,y=ukacc$front,col="red"))+geom_line()
  +geom_vline(xintercept=as.numeric(ukacc$date))
  
  
  p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
  p + geom_vline(xintercept = 5)
  
library(tidyr)
  
  ukacc<-separate(ukacc, date, into = c("year", "month") ) 
  
  month_agg<- aggregate(ukacc$drivers,ukacc$DriversKilled,by=list(ukacc$month),FUN=sum)
  
  ukacc$totalDriver<-ukacc$DriversKilled+ukacc$drivers
  
  year_avg<-aggregate( ukacc$totalDriver,by=list(ukacc$year),mean)
  

  ggplot(data=year_avg,aes(x=year_avg$x,y=year_avg$Group.1))+geom_boxplot()
?spread()
  
  
  ggplot(sleep, aes(x = ID, y = extra, fill = group)) + geom_bar()
  
  ggplot(sleep, aes(x = ID, y = extra, fill = group)) + geom_histogram()
  
  ggplot(sleep, aes(x = ID, y = extra, dodge = group,col=group,alpha=0.4)) + geom_col()
  
  ggplot(sleep, aes(x = ID, y = extra, fill = group)) + geom_freqpoly()
  
sir<-read.csv("C:\\Users\\kanupriya.saxena\\OneDrive - Accenture\\UpGrad\\Data Visualization\\sir.csv",header=TRUE)
 


ggplot(sir,aes(x=factor(Inns),y=Wkts,group=Wkts))+geom_boxplot()

median_agg<-aggregate(sir$Wkts, by=list(sir$Inns),median)

ggplot(sir,aes(x=Mdns,y=Econ))+geom_point()
 
 