#Loading the required libraries

library(graphics)
library(forecast)
library(tseries)
#loading the file into R

exchange_rate_data <- read.csv("exchange-rate-twi.csv", header = T, sep = ',')

nrow(exchange_rate_data)

#Converting dataframe to time series

timeser <- ts(exchange_rate_data$Exchange.Rate.TWI)
plot(timeser)

#Building final model in R

lmfit <- lm(Exchange.Rate.TWI ~ Month, data=exchange_rate_data)
globalpred <- predict(lmfit)
plot(globalpred, col='red', type = "l")

#Now, let's inspect the local component of the time series

localpred <- timeser - globalpred
plot(localpred, col='red', type = "l")

acf(localpred)
acf(localpred, type="partial")

armapred <- arima.sim(model=list(ar=0.96),n=304)

resi <- localpred - armapred
plot(resi)
kpss.test(resi)