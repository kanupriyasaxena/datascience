library(tseries)
library(dplyr)
library(forecast)
setwd("C:\\Users\\kanupriya.saxena\\OneDrive - Accenture\\UpGrad\\Time Series\\Assignment")

##Read the file
superstore<-read.csv("Global Superstore.csv",header=TRUE, sep=",", stringsAsFactors = FALSE)

##have a look at the structure of the data
str(superstore)
summary(superstore)

##check for NA in the data
sapply(superstore, function(x) sum(is.na(x)))
##NA values found in Postal Code 
##Having a closer look at postal code data
library(dplyr)
subsetDataforPC<-subset(superstore,!is.na(superstore$Postal.Code))
unique(subsetDataforPC$Market)
##[1] "US"
##It appears that the data data for US had the postal code

#########################Data Preparation Start#################################
##As the data is from different geographies
##Subsetting the data based on Market and Customer Segment

market<-unique(superstore$Market)
segment<-unique(superstore$Segment)
market_seg<-merge(market,segment)
CV <- function(mean, sd){
  (sd/mean)*100
}
for(i in 1:nrow(market_seg))
{
 dataframe<-
subset(superstore,superstore$Segment== as.character(market_seg[i,2]) & superstore$Market==as.character(market_seg[i,1]))
 ##Remove not required columns from the dataframe
 ##Row ID - As row id is not required for analysis
 ##Order Id - As order level analysis is not required
 ##Ship Date- As ship mode is a better variable to use
 ##Customer Name - As customer level analysis is not required
 ##Customer Id - As customer level analysis is not 
 ##Segment-As it is unique and captured in data frame name
 ##Market-As it is unique and captured in dataframe name
 ##Product ID - As product Name is a better variable
 colstoberemoved<-c(1,2,4,6,7,8,13,15)
 dataframe<-dataframe[,-colstoberemoved]
 ##Remove postal code from all dataframes except for US
 if(as.character(market_seg[i,1]) != 'US')
 {
   dataframe<-dataframe[,-6]
 }
 
 # make sure dates are parsed properly
 dataframe$Order.Date <- as.Date(dataframe$Order.Date, format = "%d-%m-%Y" )
 
 # add a 'Monthly' column with month breaks
 dataframe$Monthly <- as.Date(cut(dataframe$Order.Date, breaks = "months"))
 ##Drop the Order Date Colum to aggregate on Month
 dataframe<-dataframe[,-1]
 
  ##aggregate the Sales, Profit and quantity values to prepare a monthly data
 dataframe %>% group_by(dataframe$Monthly) %>% summarise(sum(dataframe$Sales),sum(dataframe$Profit),sum(dataframe$Quantity))

##calculate CV of the data frames
cvOfdataframe<-CV(mean=mean(dataframe$Profit), sd(dataframe$Profit))
##Add CV as a column in the market_seg table
market_seg[i,3]<-cvOfdataframe
assign(paste(market_seg[i,1],market_seg[i,2],sep="_"),dataframe) 
}

#########################Data Preparation End#################################

##Find the top 2 performing segments
top_n(market_seg,2)

##1 EMEA   Corporate 2203.832
##2 EMEA Home Office 2180.053



##Create time series for the top2 market segments
Sales_ts_EMEA_Corporate<-ts(EMEA_Corporate$Sales)
Sales_ts_EMEA_Home_Office<-ts(`EMEA_Home Office`$Sales)

##plot the time series
plot(Sales_ts_EMEA_Corporate)
plot(Sales_ts_EMEA_Home_Office)
##the time series is an exponentially decaying time series

##taking the log
plot(log(Sales_ts_EMEA_Corporate))
plot(log(Sales_ts_EMEA_Home_Office))


#########################Classical Decompositionfor EMEA corporate Start#################################

##Sampling the data for modelling
indata_EMEACorp<-EMEA_Corporate[1:1000,]
outdata_EMEACorp<-EMEA_Corporate[1001:1574,]

plot(log(Sales_ts_EMEA_Corporate))
timeser<-ts(log(indata_EMEACorp$Sales))
#####Smoothening for EMEA Corporate#####

##As the plot of log is linear. Using Moving average method for smoothening
##As no specific spikes towards the corners for no need of special smoothening at start and end
##Using w=1
w <-1
smoothedseries_1 <- stats::filter(timeser,filter=rep(1/(2*w+1),(2*w+1)),method='convolution', sides=2)
lines(smoothedseries_1,col="red")

##Using w=10
w <-10
smoothedseries_10 <- stats::filter(timeser,filter=rep(1/(2*w+1),(2*w+1)),method='convolution', sides=2)
lines(smoothedseries_10,col="blue")

##As w=10 is giving a better fit, continuing with smoothedseries_10


#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe
timevals_in <- indata_EMEACorp$Monthly
smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries_10)))
colnames(smootheddf) <- c('Month', 'Sales')
smootheddf<-smootheddf[-which(is.na(smootheddf$Sales)),]

##Fit a trend
params<-Modelling_fun()
lmfit <- lm(Sales ~ sin(params*Month)+cos(params*Month ) + Month, data=smootheddf)
global_pred <- predict(lmfit, Month=smootheddf$Month)
summary(global_pred)
plot(log(Sales_ts_EMEA_Corporate))
lines(global_pred, col="blue", lwd=2)
timeser<-ts(smootheddf$Sales)
##find the local predict value
local_pred <- timeser-global_pred
##The acf and pacf plot for local_pred shows that it needs AR and MA adjustment
acf(local_pred,type="correlation",  lag.max=40, plot=T,main="ACF Plot for local fit without ARMA")
acf(local_pred,type="partial", lag.max=40, plot=T, main="PACF Plot for local fit without ARMA")

##Trying armafit
armafit1 <- arima(local_pred, order = c(0,1,1))
tsdiag(armafit1)
resi <- local_pred-fitted(armafit1)
acf(resi,type="correlation",  lag.max=40, plot=T,main="ACF Plot for armafit1")
acf(resi,type="partial",  lag.max=40, plot=T, main="PACF Plot for armafit1")

##Both ACF and PACF plot are acceptable for armafit1
resi <- local_pred-fitted(armafit1)
adf.test(resi,alternative = "stationary")
kpss.test(resi)
armafit1
##log liklihood is -848.5
##KPSS 0.1
##ADF 0.01

##Trying armafit2 to minimize the negative log liklihood

armafit2 <- arima(local_pred, order = c(2,2,2))
tsdiag(armafit2)
resi <- local_pred-fitted(armafit2)
acf(resi,type="correlation",  lag.max=40, plot=T,main="ACF Plot for armafit2")
acf(resi,type="partial",  lag.max=40, plot=T, main="PACF Plot for armafit2")

##Both ACF and PACF plot are acceptable for armafit2
resi <- local_pred-fitted(armafit2)
adf.test(resi,alternative = "stationary")
kpss.test(resi)
armafit2
##log liklihood is -851.5
##KPSS 0.1
##ADF 0.01

##Tried multiple values of p,q and seasonality
##armafit1 seems to be the best performing


timevals_out <- outdata_EMEACorp$Month
dataframe_timeout<-as.data.frame(cbind(timevals_out))
global_pred_out <- predict(lmfit,Month=dataframe_timeout)
fcast <- global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE

accuracy <- accuracy(fcast,outdata_EMEACorp[,9])
accuracy

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred <- c(ts(global_pred),ts(global_pred_out))

plot(log(Sales_ts_EMEA_Corporate))
lines(class_dec_pred, col = "red")

#ME     RMSE      MAE      MPE     MAPE
#15.33 23.08      15.45    58.55   63.16635
#########################Classical Decompositionfor EMEA corporate End#################################
#########################Auto Arima for EMEA corporate Start#################################

##EMEA_Corporate$Sales

timeser_indata<-ts(indata_EMEACorp$Sales)
plot(log(timeser_indata))

autoarima <- auto.arima(log(timeser_indata))
autoarima
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima),col="red")

fcast_auto_arima <- forecast(autoarima, h = 6)
fcast_auto_arima
##Point      Forecast
# 1001       3.307047
# 1002       3.303099
# 1003       3.299151
# 1004       3.295204
# 1005       3.291256
# 1006       3.287308

Accuracy <- accuracy(fcast_auto_arima$fitted,outdata_EMEACorp$Sales)
Accuracy

#ME     RMSE      MAE      MPE     MAPE
#14.64  22.37     14.75    55.66   59.64252
#########################Auto Arima for EMEA corporate End#################################





#########################Classical Decompositionfor EMEA Home Office Start#################################

##Sampling the data for modelling

indata_EMEAHom<-`EMEA_Home Office`[1:850,]
outdata_EMEAHom<-`EMEA_Home Office`[851:917,]

plot(log(Sales_ts_EMEA_Home_Office))
timeser<-ts(log(indata_EMEAHom$Sales))
#####Smoothening for EMEA Home Office#####

##As the plot of log is linear. Using Moving average method for smoothening
##As no specific spikes towards the corners for no need of special smoothening at start and end
##Using w=1
w <-1
smoothedseries_1 <- stats::filter(timeser,filter=rep(1/(2*w+1),(2*w+1)),method='convolution', sides=2)
lines(smoothedseries_1,col="red")

##Using w=10
w <-10
smoothedseries_10 <- stats::filter(timeser,filter=rep(1/(2*w+1),(2*w+1)),method='convolution', sides=2)
lines(smoothedseries_10,col="blue")

##As w=10 is giving a better fit, continuing with smoothedseries_10


#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe
timevals_in <- indata_EMEAHom$Monthly
smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries_10)))
colnames(smootheddf) <- c('Month', 'Sales')
smootheddf<-smootheddf[-which(is.na(smootheddf$Sales)),]
timeser<-ts(smootheddf$Sales)
##Fit a trend
params<-Modelling_fun()
lmfit <- lm(Sales ~ sin(params*Month)+cos(params*Month ) + Month, data=smootheddf)
global_pred <- predict(lmfit, Month=smootheddf$Month)
summary(global_pred)
plot(log(Sales_ts_EMEA_Home_Office))
lines(global_pred, col="blue", lwd=2)

##find the local predict value
local_pred <- timeser-global_pred
##The acf and pacf plot for local_pred shows that it needs AR and MA adjustment
acf(local_pred,type="correlation",  lag.max=40, plot=T,main="ACF Plot for local fit without ARMA")
acf(local_pred,type="partial", lag.max=40, plot=T, main="PACF Plot for local fit without ARMA")

##Trying armafit
armafit1 <- arima(local_pred, order = c(0,1,1))
tsdiag(armafit1)
resi <- local_pred-fitted(armafit1)
acf(resi,type="correlation",  lag.max=40, plot=T,main="ACF Plot for armafit1")
acf(resi,type="partial",  lag.max=40, plot=T, main="PACF Plot for armafit1")

##Both ACF is acceptable PACF plot is a little out of range
resi <- local_pred-fitted(armafit1)
adf.test(resi,alternative = "stationary")
kpss.test(resi)
armafit1
##log liklihood is 448.59
##KPSS 0.1
##ADF 0.01

##Trying armafit2 to minimize the negative log liklihood

armafit2 <- arima(local_pred, order = c(5,2,5))
tsdiag(armafit2)
resi <- local_pred-fitted(armafit2)
acf(resi,type="correlation",  lag.max=40, plot=T,main="ACF Plot for armafit2")
acf(resi,type="partial",  lag.max=40, plot=T, main="PACF Plot for armafit2")

##Both ACF and PACF plot are acceptable for armafit2
resi <- local_pred-fitted(armafit2)
adf.test(resi,alternative = "stationary")
kpss.test(resi)
armafit2
##log liklihood is 456.84
##KPSS 0.1
##ADF 0.01


##Tried multiple values of p,q and seasonality
##armafit2 seems to be the best performing due to higher log liklihood


timevals_out <- outdata_EMEAHom$Month
dataframe_timeout<-as.data.frame(cbind(timevals_out))
global_pred_out <- predict(lmfit,Month=dataframe_timeout)
fcast <- global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE

accuracy <- accuracy(fcast,outdata_EMEACorp[,9])
accuracy

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred <- c(ts(global_pred),ts(global_pred_out))

plot(log(Sales_ts_EMEA_Corporate))
lines(class_dec_pred, col = "red")

#ME     RMSE      MAE      MPE     MAPE
#15.93 23.49      16.00    63.87   66.61
#########################Classical Decompositionfor Home Office End#################################




#########################Auto Arima for EMEA Home Office Start#################################
##EMEA_Home_Officee$Sales
indata_EMEAHom<-`EMEA_Home Office`[1:850,]
outdata_EMEAHom<-`EMEA_Home Office`[851:917,]
timeser_indata<-ts(indata_EMEAHom$Sales)
plot(log(timeser_indata))

autoarima <- auto.arima(log(timeser_indata))
autoarima
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima),col="red")

fcast_auto_arima <- forecast(autoarima, h = 6)
fcast_auto_arima
##Point      Forecast
# 851       2.210693
# 852       2.204890
# 853       2.199086
# 854       2.193283
# 855       2.187479
# 856       2.181676


Accuracy <- accuracy(fcast_auto_arima$fitted,outdata_EMEAHom$Sales)
Accuracy

#ME     RMSE      MAE      MPE     MAPE
#3.67  12.60      5.87    -12.08   52.19
#########################Auto Arima for EMEA Home Office End#################################

######################################################
#####Following code is to find best sinusoid function
######################################################

Modelling_fun <- function(x) {
  y<-smootheddf$Sales
  t<-smootheddf$Month
  ssp <- spectrum(y) 
  per <- 1/ssp$freq[ssp$spec==max(ssp$spec)]
  g_lmfit <<- lm(y ~ sin(2*pi/per*t)+cos(2*pi/per*t ) + t)
  summary(g_lmfit)
  rg <- diff(range(smootheddf$Sales))
  plot(~t,ylim=c(min(y)-0.1*rg,max(y)+0.1*rg))
  lines(fitted(g_lmfit)~smootheddf$Month,col=4,lty=2)
  return(2*pi/per)
}







