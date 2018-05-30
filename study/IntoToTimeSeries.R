setwd("C:\\Users\\kanupriya.saxena\\OneDrive - Accenture\\UpGrad\\Time Series")
install.packages("Graphics")
install.packages("forecast")

exchange_rate_data <- read.csv("exchange-rate-twi.csv", header = T, sep = ',')

nrow(exchange_rate_data)

timeser <- ts(exchange_rate_data$Exchange.Rate.TWI)
plot(timeser)

exchange_rate_data$globalpred<- 111.82-0.2178*exchange_rate_data$Month
exchange_rate_data$localpred<-exchange_rate_data$Exchange.Rate.TWI-exchange_rate_data$globalpred

exchange_rate_data[which(exchange_rate_data$Month==300),]