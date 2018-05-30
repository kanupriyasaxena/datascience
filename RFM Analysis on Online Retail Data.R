setwd("C:\\Users\\kanupriya.saxena\\OneDrive - Accenture\\UpGrad\\KS Stats and RFM")
Online.Retail <- read.csv("Online Retail.csv", stringsAsFactors=FALSE)
order_wise <- na.omit(Online.Retail)

#Monetary aspect of RFM
order_wise$dollarValue<-order_wise$Quantity*order_wise$UnitPrice
monetary<-aggregate(dollarValue~CustomerID,order_wise,sum )

#Frequency aspect of RFM
frequency<-order_wise[,c(7,1)]
temp<-table(as.factor(frequency$CustomerID))
temp<-data.frame(temp)
colnames(temp)[1]<-c("CustomerID")

#merge with monetary
monetary<-merge(monetary,temp,by="CustomerID")
library(stringr)
#Recency Aspect of RFM
recency<-order_wise[,c(7,5)]
recency$Invoicedate<- str_replace_all(recency$Invoicedate, pattern="\\/", replacement ="-")
recency$InvoiceDate<-as.Date(recency$InvoiceDate, "%d/%m/%Y %H:%M")

#Calculate the recency wrt to the latest date
maxDate<-max(recency$InvoiceDate)
maxDate
maxDate<-maxDate+1

#For each row calculate the difference with the date
recency$recencyIndex<-maxDate-recency$InvoiceDate
recency<-aggregate(recency$recencyIndex, by=list(recency$CustomerID),FUN="min")
colnames(recency)[1]<- "CustomerID"
colnames(recency)[2]<- "Recency"


RFM <- merge(monetary, recency, by = ("CustomerID"))
RFM$Recency <- as.numeric(RFM$Recency)

RFM<-monetary

box <- boxplot.stats(RFM$Amount)
out <- box$out
RFM1 <- RFM[ !RFM$Amount %in% out, ]
RFM <- RFM1
box <- boxplot.stats(RFM$Freq)
out <- box$out
RFM1 <- RFM[ !RFM$Freq %in% out, ]
RFM <- RFM1
box <- boxplot.stats(RFM$Recency)
out <- box$out
RFM1 <- RFM[ !RFM$Recency %in% out, ]
RFM <- RFM1


summary(RFM)

summary(monetary)


