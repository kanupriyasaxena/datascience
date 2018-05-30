#install.packages("caTools")
library(caTools)

setwd("C:\\Users\\kanupriya.saxena\\OneDrive - Accenture\\UpGrad\\Logistic Regression\\pima india diabetes case study")

#read the file
pima_indian_diabetes<-read.csv("pima_indian_diabetes.csv",stringsAsFactors = FALSE)

#have a lok at the data
str(pima_indian_diabetes)
summary(pima_indian_diabetes)

#scale the data
pima_indian_diabetes$No_Times_Pregnant<- scale(pima_indian_diabetes$No_Times_Pregnant)
pima_indian_diabetes$Plasma_Glucose<- scale(pima_indian_diabetes$Plasma_Glucose)
pima_indian_diabetes$Diastolic_BP<- scale(pima_indian_diabetes$Diastolic_BP)
pima_indian_diabetes$Triceps<- scale(pima_indian_diabetes$Triceps)
pima_indian_diabetes$Insulin<- scale(pima_indian_diabetes$Insulin)
pima_indian_diabetes$BMI<- scale(pima_indian_diabetes$BMI)
pima_indian_diabetes$Age<- scale(pima_indian_diabetes$Age)

#have a look at the summary
summary(pima_indian_diabetes)

#set seed and sample the data in train ans test
set.seed(100)
indices = sample.split(pima_indian_diabetes$Diabetes, SplitRatio = 0.7)

train = pima_indian_diabetes[indices,]

test = pima_indian_diabetes[!(indices),]

#run the regression

model1<-glm(Diabetes~.,data = train,family = "binomial")

summary(model1)

library("MASS")

#stepAIC
model2<- stepAIC(model1, direction="both")

summary(model2)

library(car)

#check VIF
vif(model2)