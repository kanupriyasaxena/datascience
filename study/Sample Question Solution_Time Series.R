setwd("C:\\Users\\kanupriya.saxena\\OneDrive - Accenture\\UpGrad\\Time Series")


exchange_rate_data <- read.csv("exchange-rate-twi.csv", header = T, sep = ',')

nrow(exchange_rate_data)

#Converting dataframe to time series

timeser <- ts(exchange_rate_data$Exchange.Rate.TWI)
plot(timeser)

#Building final model in R

lmfit <- lm(Exchange.Rate.TWI ~ Month, data=exchange_rate_data)
globalpred <- predict(lmfit)


#Now, let's inspect the local component of the time series

localpred <- timeser - globalpred
# Generate a sequence of Gaussian random numbers and
# convert the sequence into a time-series object
noise <- ts(rnorm(200, mean = 0, sd = 1))


# Plot the time series
plot.ts(noise)


# Compute and plot the ACF for the time series
acf(localpred, level=95, lag.max=40, main="ACF Plot for White Noise")


# Compute and plot the ACF for the time series
pacf(localpred, level=95, lag.max=40, main="PACF Plot for White Noise")


armapred <- arima.sim(model=list(ar=0.96),n=304)

resi <- localpred - armapred

install.packages("tseries")
library(tseries)

kpss.test(resi)