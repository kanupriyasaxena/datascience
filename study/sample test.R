library(tidyr)
on_pop<-read.csv(file="C:\\Users\\kanupriya.saxena\\Desktop\\Study\\UpGrad\\DataPrepAndModelEval\\OnlineNewsPopularity\\OnlineNewsPopularity.csv", header = TRUE)
summary(on_pop)
long_on_pop<-gather(on_pop,day,hit,weekday_is_monday:weekday_is_sunday)
long_on_pop<-long_on_pop[!(long_on_pop$hit==0),]
library(dplyr)
group1<-group_by(long_on_pop,is_weekend)
summary1<-summarise(group1,mean(shares),sum(hit))

group2<-group_by(long_on_pop,day)
summary2<-summarise(group2,mean(shares))
summary(summary2)
summary2<-arrange(summary2,desc(summary2$`mean(shares)`))

long_channel<-gather(long_on_pop,channel,occurence,data_channel_is_lifestyle:data_channel_is_world)
long_channel<-long_channel[!(long_channel$occurence==0),]

group3<-group_by(long_channel,channel)
summary3<-summarise(group3,mean(shares))

summary3<-arrange(summary3,desc(summary3$`mean(shares)`))