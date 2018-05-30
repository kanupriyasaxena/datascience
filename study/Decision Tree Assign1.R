setwd("C:\\Users\\kanupriya.saxena\\OneDrive - Accenture\\UpGrad\\Decision Tree")

heart<-read.csv("heart.csv",header = TRUE, stringsAsFactors = FALSE)
install.packages("rpart")
install.packages("rpart.plot")

library(rpart)
library(rpart.plot)

heart$heart.disease<-as.factor(heart$heart.disease)
str(heart)
tree = rpart(heart.disease~., data=heart)
prp(tree)
