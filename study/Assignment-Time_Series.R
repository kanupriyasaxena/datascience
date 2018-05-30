############################# Setting the working directory ####################################
#setwd("E:/Shikhab/Documents/shikhab/prsnl/IIITB-PGDD/workspace/Course 4 - Predictive Analysis II/Time Series/Retail-Giant Sales Forecasting Assignment")

###############################################################################################
############################ Retail-Giant Sales Forecasting.  #################################
###############################################################################################
# 1. Business Understanding
# 2. Data Understanding
# 3. Data Preparation
# 4. Model Building 
# 5  Model Evaluation

###############################################################################################
########################## 1. Business Understanding: ######################################### 
###############################################################################################
#"Global Mart" is an online store super giant having worldwide operations. 
# It takes orders and delivers across the globe and deals with all the major product categories 
# - consumer, corporate & home office.
#
# The Objective is to finalise the plan for the next 6 months. 
# So, you want to forecast the sales and the demand for the next 6 months, 
# that would help you manage the revenue and inventory accordingly.
##############################################################################################


##############################################################################################
############################## 2. Data Understanding #########################################
##############################################################################################

##############################################################################################
####################### Install and Load the required packages ###############################
#############################################################################################

packagesRequired = c('tseries','forecast','dplyr','ggplot2','stringr','graphics')

#Installing the required packages
for(pkg in packagesRequired){
  if(!require(pkg,character.only = TRUE)){
    install.packages(pkg,character.only = TRUE)
  }
  library(pkg,character.only = TRUE)
}

##############################################################################################
##################################### LOADING THE INPUT FILES ###############################
#############################################################################################

global_data<-read.csv("Global Superstore.csv",header=TRUE, sep=",", stringsAsFactors = FALSE)

#############################################################################################
#####   Data Understanding: 
#############################################################################################
#The data currently has the transaction level data, where each row represents a particular 
#order made on the online store. There are 24 attributes related to each such transaction. 
#The "Market" attribute has 7-factor levels representing the geographical market sector that 
#the customer belongs to. The "Segment" attribute tells which of the 3 segments that customer
#belongs to.

#Understanding Dimensions
dim(global_data)    #24 attributes , 51290 transactions

##Have a look at the structure of the data
str(global_data)

#Printing first few rows
head(global_data)

#Exploring the data

summary(global_data)

##check for NA in the data
sapply(global_data, function(x) sum(is.na(x)))

#Check for duplicate records
sum(duplicated(global_data))  # there are no duplicate records in training data

##NA values found in Postal Code 
##Having a closer look at postal code data
subsetDataforPC<-subset(global_data,!is.na(global_data$Postal.Code))
unique(subsetDataforPC$Market)
##[1] "US"
##It appears that the data data for US had the postal code

###############################################################################################
########################## 3. Data Preparation: ######################################### 
###############################################################################################
##As the data is from different geographies
##Subsetting the data based on Market and Customer Segment

market<-unique(global_data$Market)
segment<-unique(global_data$Segment)
unique(global_data$Category)
#market_seg<-merge(market,segment)
#Clean segment column, replace space by "_" so that it can be used later
global_data$Segment<-str_replace_all(global_data$Segment, " ",  "_")

#Create New column to create 21 segment market+segment
global_data$mkt_seg<-paste(global_data$Market, global_data$Segment,sep='_')
unique(global_data$mkt_seg)

#take only required columns
#Create a field YearMonth (YYYYMM) for aggregation on Order_date

# convert time columns to datetime oject
global_data$Order.Date<-as.POSIXlt(global_data$Order.Date,format="%d-%m-%Y")

#Extract the hour and day data from the request time
global_data$Month <- format(global_data$Order.Date, "%m")
global_data$Year <- format(global_data$Order.Date, "%Y")

#Create New column YearMonth (YYYYMM) for aggregation 
global_data$YearMonth<-as.integer(paste(global_data$Year, global_data$Month,sep=''))
global_data1<-subset(global_data, select=c("YearMonth","mkt_seg", "Quantity","Sales","Profit"))

##Aggregate on market segment and Months
global_data2 <- 
  global_data1 %>%
  group_by(mkt_seg,YearMonth) %>% 
  summarise(Quantity=sum(Quantity),Sales = sum(Sales), Profit=sum(Profit))


#To find consistent and most profitable segments (CV is coeffient of variation)
Profit_CV_Data <- 
  global_data2 %>%
  group_by(mkt_seg) %>% 
  summarise(Profit_By_Seg=sum(Profit),
            CV = 100*sd(Profit)/mean(Profit)
  )

View(Profit_CV_Data)

ggplot(Profit_CV_Data, aes(mkt_seg, CV)) + geom_bar(position = "dodge",stat = "identity")
ggplot(global_data, aes(Segment, Profit, fill=Market)) + geom_bar(position = "dodge",stat = "identity")
ggplot(global_data, aes(Segment, Quantity, fill=Market)) + geom_bar(position = "dodge",stat = "identity")
ggplot(global_data, aes(Segment, Sales, fill=Market)) + geom_bar(position = "dodge",stat = "identity")


##High Profit and low Coefficient of Variation
##Two top Segments are:
#1st Segment: EU_Consumer
#2nd Segment: APAC_Consumer

#1.Function to Create Time Series for particular market Segment
ts_mkt_seg_fun <- function(mkt_seg, Var1) {
  print(paste0("Calling function to create Time series of ", Var1, " for ",mkt_seg ))
  
  #mkt_seg <- "APAC_Consumer"
  #Var1<-"Sales"
  
  subset_data<-subset(global_data2[,c("YearMonth", Var1)], global_data2$mkt_seg== mkt_seg)
  g_all_subset_data <<- subset_data
  colnames(subset_data) <- c("Month","forecast_variable")
  subset_data$Month <- c(1:nrow(subset_data))
  subset_data <- subset_data[1:42,]
  g_subset_data<<- subset_data  #Global data set for use in other functions
  ts_data <- ts(subset_data$forecast_variable)
  plot(ts_data,ylab = paste0(Var1))
  title(paste0(mkt_seg , " " ,Var1," forecast"), 
        cex.main = 2,   font.main= 3, col.main= "black"
  )
  print(ts_data)
  return(ts_data)
}

#2 Function to Smooth Time Series
smooth_ts_fun <- function (ts_data,mkt_seg,Var1){
  print(paste0("Calling function to Smoothen the Time series of ", Var1, " for ",mkt_seg))
  w <-1
  s_ts_data <- stats::filter(ts_data, 
                             filter=rep(1/(2*w+1),(2*w+1)), 
                             method='convolution', sides=2)
  
  #Smoothing left end of the time series
  diff <- s_ts_data[w+2] - s_ts_data[w+1]
  for (i in seq(w,1,-1)) {
    s_ts_data[i] <- s_ts_data[i+1] - diff
  }
  
  #Smoothing right end of the time series
  n <- length(ts_data)
  diff <- s_ts_data[n-w] - s_ts_data[n-w-1]
  for (i in seq(n-w+1, n)) {
    s_ts_data[i] <- s_ts_data[i-1] + diff
  }
  #Plot the smoothed time series
  g_timeval_in <<- g_subset_data$Month #Global Variable
  lines(s_ts_data, col="blue", lwd=2)
  title(sub=paste0("Smoothened Time Series"),
        cex.sub = 1,   font.sub= 1, col.sub= "blue")
  print(s_ts_data)
  return(s_ts_data)
}

#3 Modelling_fun
Modelling_fun <- function(mkt_seg,Var1) {
  print(paste0("Calling the Modelling function for ",mkt_seg ," and ", Var1))
  ######################################################
  #####Following code is to find best sinusoid function
  #Now, let's fit a multiplicative model with trend and seasonality to the data
  #Seasonality will be modeled using a sinusoid function
  ######################################################
  y<-g_subset_data$forecast_variable
  t<-g_subset_data$Month
  ssp <- spectrum(y) 
  per <- 1/ssp$freq[ssp$spec==max(ssp$spec)]
  g_lmfit <<- lm(y ~ sin(2*pi/per*t)+cos(2*pi/per*t ) + t)
  summary(g_lmfit)
  
  rg <- diff(range(g_subset_data$forecast_variable))
  plot(~t,ylim=c(min(y)-0.1*rg,max(y)+0.1*rg))
  lines(fitted(g_lmfit)~g_subset_data$Month,col=4,lty=2)
  title(main=paste0("Seasonality and Trend for " ,mkt_seg ," and ", Var1),
        cex.main = 2,   font.main= 2, col.main= "blue")
  
  
}

#4 Classical decomposition function
classical_Decom_fun <- function(mkt_seg,Var1) {
  print(paste0("Calling the Classical decomposition function for ",mkt_seg ," and ", Var1))
  #EXtract Global pattern
  global_pred <- predict(g_lmfit, Month=g_timeval_in)
  summary(global_pred)
  lines(g_timeval_in, global_pred, col='blue', lwd=2)
  
  #Now, let's look at the locally predictable series
  #We will model it as an ARMA series
  local_pred <- g_ts_subset_data - global_pred
  plot(local_pred, col='red', type = "l")
  title(main=paste0("Locally Predictable Series for " ,mkt_seg ," and ", Var1),
        cex.main = 2,   font.main= 2, col.main= "blue")
  
  acf(local_pred)
  acf(local_pred, type="partial")
  armafit <- auto.arima(local_pred)
  
  tsdiag(armafit)
  armafit
  print(armafit)
  
  #We'll check if the residual series is white noise
  #	ADF test: Null hypothesis assumes that the series is not stationary
  #	KPSS test: Null hypothesis assumes that the series is stationary
  
  resi <- local_pred - fitted(armafit)
  plot(resi)
  adf.test(resi,alternative = "stationary")
  kpss.test(resi)
  summary(resi)
  
  #p value for ADF = 0.01 =< 0.01 -- Reject Null hypothesis i.e. it a stationary series
  #p value for KPSS= 0.1 >0.01 - Fail to reject Null hypothesis i.e. it a stationary series
  
  #Now, let's evaluate the model using MAPE
  #First, let's make a prediction for the last 6 months
  outdata <- as.data.frame(g_all_subset_data[43:48,])
  timevals_out <- outdata$Month
  global_pred_out <- predict(g_lmfit,data.frame(Month = timevals_out))
  fcast <- global_pred_out
  
  #Now, let's compare our prediction with the actual values, using MAPE
  MAPE_class_dec <- forecast::accuracy(fcast,outdata[,2])
  MAPE_class_dec
  print(MAPE_class_dec)
  
  #Let's also plot the predictions along with original values, to
  #get a visual feel of the fit
  class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
  g_class_dec_pred <<- class_dec_pred
  class_dec_pred
  print(class_dec_pred)
  plot(g_ts_subset_data, col = "black",ylab=paste0(Var1))
  lines(class_dec_pred, col = "red")
  title(main=paste0("Linear Model Forecast" ),
        cex.main = 2,   font.main= 2, col.main= "blue")
  title(sub=paste0(Var1 , " for " , mkt_seg ),
        cex.main = 1,   font.main= 1, col.main= "red")
  
}

#5 Arima function
arima_fit_fun <- function(mkt_seg,Var1) {
  
  print(paste0("Calling the Auto Arima function for ",mkt_seg ," and ", Var1))
  outdata <- as.data.frame(g_all_subset_data[43:48,])
  autoarima <<- auto.arima(g_ts_subset_data)
  autoarima
  tsdiag(autoarima)
  plot(autoarima$x, col="black")
  lines(fitted(autoarima), col="red")
  
  #Again, let's check if the residual series is white noise
  resi_auto_arima <- g_ts_subset_data - fitted(autoarima)
  adf.test(resi_auto_arima,alternative = "stationary")
  kpss.test(resi_auto_arima)
  
  #Also, let's evaluate the model using MAPE
  fcast_auto_arima <- predict(autoarima, n.ahead = 6)
  MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,outdata[,2])
  MAPE_auto_arima
  
  #Lastly, let's plot the predictions along with original values, to
  #get a visual feel of the fit
  auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
  plot(g_ts_subset_data, col = "black",ylab=paste0(Var1))
  lines(auto_arima_pred, col = "red")
  title(main=paste0("ARIMA Model Forecast" ),
        cex.main = 2,   font.main= 2, col.main= "blue")
  title(sub=paste0(Var1 , " for " , mkt_seg ),
        cex.sub = 1,   font.sub= 1, col.sub= "red")
  return(autoarima)
  }

###################################################
#6 Function to predict future value for classical decomposition
future_class_predict_fun <- function (){
  ##############Future 6 months prediction #####
  autoarima_ts <- auto.arima(g_ts_subset_data)
  tsdiag(autoarima_ts)
  future_forecast<-forecast(autoarima_ts,h=6)
  plot(future_forecast)
  
}



##################################################
#####Here Process APAC_Consumer Sales Data #########
##################################################
par(mfrow=c(3,3))
#Call function ts_mkt_seg to create a time series for Sales for EU_Consumer
#ts_mkt_seg <- ts_mkt_seg_fun("EU_Consumer","Sales")
g_ts_subset_data <<- ts_mkt_seg_fun("APAC_Consumer","Sales")
#Call function to smoothen the time series
smooth_ts_mkt_seg <- smooth_ts_fun(g_ts_subset_data,"APAC_Consumer","Sales")
#Convert the time series to a dataframe
smooth_ts_mkt_seg_df <<- as.data.frame(cbind(g_timeval_in, as.vector(smooth_ts_mkt_seg)))
colnames(smooth_ts_mkt_seg_df) <- c('Month', 'forecast_variable')

Modelling_fun("APAC_Consumer","Sales")
classical_Decom_fun("APAC_Consumer","Sales")
arima_fit_fun("APAC_Consumer","Sales")
future_class_predict_fun()
####################################


##################################################
#####Here Process APAC_Consumer Quantity Data #########
##################################################
par(mfrow=c(3,3))
#Call function ts_mkt_seg to create a time series for Quantity for EU_Consumer
g_ts_subset_data <<- ts_mkt_seg_fun("APAC_Consumer","Quantity")
#Call function to smoothen the time series
smooth_ts_mkt_seg <<- smooth_ts_fun(g_ts_subset_data,"APAC_Consumer","Quantity")
#Convert the time series to a dataframe
smooth_ts_mkt_seg_df <<- as.data.frame(cbind(g_timeval_in, as.vector(smooth_ts_mkt_seg)))
colnames(smooth_ts_mkt_seg_df) <- c('Month', 'forecast_variable')
Modelling_fun("APAC_Consumer","Quantity")
classical_Decom_fun("APAC_Consumer","Quantity")
arima_fit_fun("APAC_Consumer","Quantity")
future_class_predict_fun()
####################################


##################################################
#####Here Process EU_Consumer Sales Data #########
##################################################
par(mfrow=c(3,3))
#Call function ts_mkt_seg to create a time series for Sales for EU_Consumer
g_ts_subset_data <<- ts_mkt_seg_fun("EU_Consumer","Sales")

#Call function to smoothen the time series
smooth_ts_mkt_seg <<- smooth_ts_fun(g_ts_subset_data,"EU_Consumer","Sales")
#Convert the time series to a dataframe
smooth_ts_mkt_seg_df <<- as.data.frame(cbind(g_timeval_in, as.vector(smooth_ts_mkt_seg)))
colnames(smooth_ts_mkt_seg_df) <- c('Month', 'forecast_variable')
Modelling_fun("EU_Consumer","Sales")

classical_Decom_fun("EU_Consumer","Sales")
arima_fit_fun("EU_Consumer","Sales")
future_class_predict_fun()
####################################

##################################################
#####Here Process EU_Consumer Quantity Data #########
##################################################
par(mfrow=c(3,3))
#Call function ts_mkt_seg to create a time series for Sales for EU_Consumer
g_ts_subset_data <<- ts_mkt_seg_fun("EU_Consumer","Quantity")
#Call function to smoothen the time series
smooth_ts_mkt_seg <<- smooth_ts_fun(g_ts_subset_data,"EU_Consumer","Quantity")
#Convert the time series to a dataframe
smooth_ts_mkt_seg_df <<- as.data.frame(cbind(g_timeval_in, as.vector(smooth_ts_mkt_seg)))

colnames(smooth_ts_mkt_seg_df) <- c('Month', 'forecast_variable')
Modelling_fun("EU_Consumer","Quantity")
classical_Decom_fun("EU_Consumer","Quantity")
arima_fit_fun("EU_Consumer","Quantity")
future_class_predict_fun()
####################################

