setwd("C:\\Users\\kanupriya.saxena\\OneDrive - Accenture\\UpGrad\\Time Series")
bitcoin<- read.csv("bitcoin_price_historical_data.csv",header = T,sep = ',')

timeseries<-ts(bitcoin)
plot(bitcoin,type="l")

##Plot seems exponential
##converting it into log


bitcoin$logPrice<-log(bitcoin$Price)

model<-lm(logPrice~.,data=bitcoin)
globalpred<-predict(model)

bitcoin<-cbind(bitcoin,globalpred)

bitcoin$localpred<-bitcoin$logPrice-bitcoin$globalpred



