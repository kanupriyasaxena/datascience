# The following code provides a basic model building template.
# Feel free to tweak the code as this is just for practice purpose.
# But keep in mind that the aim is to compare all the three models based on accuracies.
setwd("C:\\Users\\kanupriya.saxena\\OneDrive - Accenture\\UpGrad\\Random Forest")
# Read data
consumers <- read.csv("ecommerce_consumers.csv")
str(consumers)

# EDA and data Visualisation

# Modelling -------------------------------------------------

## Divide the data into train and test in 75:25 ratio.
set.seed(10)

## Build a logistic regression model
logistic_model <-
logistic_predictions <-
logistic_confusion_matrix <-
  
## Build a decision tree model and try different values of hyperparameters
tree_model <-
tree_predictions <-
tree_confusion_matrix <-
  
## Build an SVM model with different kernels and try different values of hyperparameters
svm_model <-
svm_predictions <-
svm_confusion_matrix <-

  

  
  
  