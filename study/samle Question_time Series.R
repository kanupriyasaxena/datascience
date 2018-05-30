setwd("C:\\Users\\kanupriya.saxena\\OneDrive - Accenture\\UpGrad\\Time Series")

bitcoin<-read.csv("bitcoin_price_historical_data.csv", header = T)

timeser<-ts(bitcoin$Price)

indata<-bitcoin[1:29,]
outdata<-bitcoin[30:32,]

timeser_indata<-ts(indata$Price)

plot(timeser_indata)



autoarima <- auto.arima(timeser_indata)
autoarima
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima),col="red")

fcast_auto_arima <- predict(autoarima, n.ahead = 3)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,outdata[,2])[5]
MAPE_auto_arima

