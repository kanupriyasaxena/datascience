setwd("C:\\Users\\kanupriya.saxena\\OneDrive - Accenture\\UpGrad\\SVM\\Assignment")


############################ SVM Digit Recogniser #################################
# 1. Business Understanding
# 2. Data Understanding
# 3. Data Preparation
# 4. Model Building 
#  4.1 Linear kernel
#  4.2 RBF Kernel
# 5 Hyperparameter tuning and cross validation

#####################################################################################

# 1. Business Understanding: 

#The objective is to identify each of a large number of black-and-white
#rectangular pixel displays as one of the digits

#####################################################################################

# 2. Data Understanding: 
 

#3. Data Preparation: 


#Loading Neccessary libraries

library(kernlab)
library(readr)
library(caret)



#Loading Data

train_data <- read_csv("mnist_train.csv")
test_data<- read_csv("mnist_test.csv")

#Understanding Dimensions

dim(train_data)

#Structure of the dataset

str(train_data)

#printing first few rows

head(train_data)

#Exploring the data

summary(train_data)


#checking missing value

sapply(train_data, function(x) sum(is.na(x)))
##No missing value found

#Making our target class to factor

train_data$`5`<-factor(train_data$`5`)

#rename the target column to digit
names(train_data)[names(train_data)== '5']<-"digit"
#training data has 59999 rows and 785 columns
# Split the training data further to 15% of data as there is huge training data
set.seed(100)
train.indices = sample(1:nrow(train_data), 0.15*nrow(train_data))
train = train_data[train.indices, ]
test = train_data[-train.indices, ]




#Constructing Model

#Using Linear Kernel
Model_linear <- ksvm(digit~ ., data = train, scale = FALSE, kernel = "vanilladot")
Eval_linear<- predict(Model_linear, test)

#confusion matrix - Linear Kernel
confusionMatrix(Eval_linear,test$digit)
#Accuracy : 0.911           
#95% CI : (0.9085, 0.9135)
#No Information Rate : 0.112           
#P-Value [Acc > NIR] : < 2.2e-16       
#Kappa : 0.9011 


Model_RBF <- ksvm(digit~ ., data = train, scale = FALSE, kernel = "rbfdot")
Eval_RBF<- predict(Model_RBF, test)

#confusion matrix - RBF Kernel
confusionMatrix(Eval_RBF,test$digit)
#Accuracy : 0.9558         
#95% CI : (0.954, 0.9576)
#P-Value [Acc > NIR] : < 2.2e-16      
#Kappa : 0.9509   

##The Accuracy and Kappa Values improve signifcantly with the RBF kernel

############   Hyperparameter tuning and Cross Validation #####################

# We will use the train function from caret package to perform Cross Validation. 

#traincontrol function Controls the computational nuances of the train function.
# i.e. method =  CV means  Cross Validation.
#      Number = 2 implies Number of folds in CV.

trainControl <- trainControl(method="cv", number=5)


# Metric <- "Accuracy" implies our Evaluation metric is Accuracy.

metric <- "Accuracy"

#Expand.grid functions takes set of hyperparameters, that we shall pass to our model.

set.seed(7)
grid <- expand.grid(.sigma=c(0.025, 0.05), .C=c(0.1,0.5,1,2) )


#train function takes Target ~ Prediction, Data, Method = Algorithm
#Metric = Type of metric, tuneGrid = Grid of Parameters,
# trcontrol = Our traincontrol method.

fit.svm <- train(digit~., data=train, method="svmRadial", metric=metric, 
                 tuneGrid=grid, trControl=trainControl)

print(fit.svm)

plot(fit.svm)



