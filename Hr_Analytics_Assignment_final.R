

#############################Telecom Solution###################
################################################################
#Business Understanding
#Data Understanding
#Data Preparation & EDA
#Model Building 
#Model Evaluation
################################################################

### Business Understanding:

#A large company named XYZ, employs, at any given point of time, around 4000 employees.
#  However, every year, around 15% of its employees leave the company and need to be replaced
#  with the talent pool available in the job market. The management believes that this 
#  level of attrition (employees leaving, either on their own or because they got fired) 
#  is bad for the company, because of the following reasons -
#  The former employees' projects get delayed, which makes it difficult to meet timelines, 
#  resulting in a reputation loss among consumers and partners
#  A sizeable department has to be maintained, for the purposes of recruiting new talent
#  More often than not, the new employees have to be trained for the job and/or given time 
#  to acclimatise themselves to the company

##  AIM:
# Is to model the probability of attrition using a logistic regression. 
# The results thus obtained will be used by the management to understand what 
# changes they should make to their workplace, in order to get most of their employees 
# to stay.

# The aim is to automate the process of predicting 
# if a customer would telecom or not and to find the factors affecting the telecom. 
# Whether a customer will telecom or not will depend on data from the following three buckets:

#########################################################################################
####################### Install and Load the required packages ##########################
#########################################################################################
 
packagesRequired = c('tidyr','dplyr','corrplot','MASS','car','e1071','cowplot','ggplot2','GGally',
                     'caTools','lubridate','lattice','ddalpha','caret','scales')

#Installing the required packages
for(pkg in packagesRequired){
  if(!require(pkg,character.only = TRUE)){
    install.packages(pkg,character.only = TRUE)
  }
  library(pkg,character.only = TRUE)
}

#############################################################################################
############################## Data Understanding ###########################################
#############################################################################################

#### LOADING THE INPUT FILES , considering different Na string formats to be taken as NA ####
#############################################################################################

general_data<- read.csv("general_data.csv", stringsAsFactors = F,na.strings=c(NA,"NA"," NA"))
manager_survey_data<- read.csv("manager_survey_data.csv", stringsAsFactors = F,na.strings=c(NA,"NA"," NA"))
employee_survey_data<- read.csv("employee_survey_data.csv", stringsAsFactors = F,na.strings=c(NA,"NA"," NA"))
in_time_data<- read.csv("in_time.csv", stringsAsFactors = F,na.strings=c(NA,"NA"," NA"))
out_time_data<- read.csv("out_time.csv", stringsAsFactors = F,na.strings=c(NA,"NA"," NA"))

#####  Understanding the data ####
#############################################################################################
#Check the structure of all datasets
str(general_data)   #4410 obs. of  24 variables:
str(manager_survey_data) #4410 obs. of  3 variables:
str(employee_survey_data) #4410 obs. of  4 variables:
str(in_time_data) #4410 obs. of  262 variables
str(out_time_data) #4410 obs. of  262 variables:

#Taking the glimpse of data
glimpse(general_data)
glimpse(manager_survey_data)
glimpse(employee_survey_data)
glimpse(in_time_data)
glimpse(out_time_data)

############################ Data Cleaning And Preparation ##################################
#############################################################################################

## The in_time_data and out_time_data has certain columns as holiday columns where all entries are NA
## Removing the holiday columns

holiday_columns<-c('X2015.01.01','X2015.01.14','X2015.01.26','X2015.03.05','X2015.05.01','X2015.07.17','X2015.09.17','X2015.10.02','X2015.11.09','X2015.11.10','X2015.11.11','X2015.12.25')
in_time_data<-in_time_data[ , !(names(in_time_data) %in% holiday_columns)]
out_time_data<-out_time_data[ , !(names(out_time_data) %in% holiday_columns)]

##The in_time and out_time of an employee which spans over 1 year can 
#be calculated as working hours by aggregating to one variable.

hr_tmp<-data.frame(matrix(nrow = 4410, ncol=250))

#Populate EmployeeID column in hr_tmp
EmployeeID = in_time_data[,1]
hr_tmp<-cbind(EmployeeID,hr_tmp)

#Calculating working hours for each day 
#######Needs to use other function like sapply as this is taking little longer to execute
for (i in 2:ncol(in_time_data)) {
  hr_tmp[,i]<-as.numeric(difftime(strptime(out_time_data[,i],format = "%Y-%m-%d %H:%M:%S"),  
                                  strptime(in_time_data[,i], format = "%Y-%m-%d %H:%M:%S"), units = "hours"))
}
#Calculating mean of working hours excluding EmployeeID  
hr_tmp$mean_work_hours <- round(rowMeans(hr_tmp[,-1],na.rm = T),2)

#Only taking EmployeeID and mean
hr_avg_hrs<-hr_tmp[,c("EmployeeID","mean_work_hours")]

##############################################################################################
########################### Preparing the dataset ###################################
##############################################################################################
length(unique(general_data$EmployeeID))         #4410
length(unique(manager_survey_data$EmployeeID))  #4410
length(unique(employee_survey_data$EmployeeID)) #4410
#CNA: Primary key is EmployeeID

#There are no column headers for in_time and out_time data,But we can safely assume (after comparision) column X is same as employeeID
length(unique(in_time_data$X)) #4410
length(unique(out_time_data$X)) #4410


#Column X in in_time_data and out_time_data is the EmployeeID
setdiff(general_data$EmployeeID, manager_survey_data$EmployeeID) # Identical EmployeeID across these datasets
setdiff(general_data$EmployeeID, employee_survey_data$EmployeeID) # Identical EmployeeID across these datasets
setdiff(general_data$EmployeeID, in_time_data$X) # Identical EmployeeID across these datasets
setdiff(in_time_data$X, out_time_data$X) # Identical X=EmployeeID across these datasets

#merge all datasets for Analysis
hr_data<-merge(x=general_data,y=manager_survey_data,by="EmployeeID")
hr_data<-merge(x=hr_data, y=employee_survey_data)
hr_data<-merge(x=hr_data, y=hr_avg_hrs,by.x="EmployeeID", by.y="EmployeeID")

############################# Derived columns ##########################################
########################################################################################

#Create a new derived column "extra_working_hrs" with 1 indicating yes and 0 as no for extra hours spent in office
hr_data$extra_working_hrs <- ifelse(hr_data$mean_work_hours>8,1,0)

#Create a new derived column "no_of_leaves" with total no of leaves taken

for(i in 1:4410)
{
  hr_data$no_of_leaves[i]<-sum(is.na(hr_tmp[i,]))
}


#########################################################################################
################## Removing non-impacting columns ##############################################
unique(hr_data$EmployeeCount) #1
unique(hr_data$StandardHours) #8
unique(hr_data$Over18) #Y

#Remove Over18, EmployeeCount & StandardHours field, it has only 1 value hence no impact on model
hr_data<-hr_data[,-c(9,16,18)]

################################################################################################################################
########################### Data Preparation & Exploratory Data Analysis
################################################################################################################################

#: Understanding the structure of the collated file
str(hr_data) #	4410 obs. of  29 variables:
#Note" Cont: Continuous variables
#     C=2: Categorical varabile with 2 values
#     C=3: Categorical variables with 3 values
#     C>3: Categorical variables with more than 3 value

# EmployeeID             : int  1 2 3 4 5 6 7 8 9 10 ... 
#  Cont Age                    : int  51 31 32 38 32 46 28 29 31 25 ...
#  C=2  Attrition              : chr  "No" "Yes" "No" "No" ...
#  C>3  BusinessTravel         : chr  "Travel_Rarely" "Travel_Frequently" "Travel_Frequently" "Non-Travel" ...
#  C>3  Department             : chr  "Sales" "Research & Development" "Research & Development" "Research & Development" ...
#  Cont DistanceFromHome       : int  6 10 17 2 10 8 11 18 1 7 ...
#  C>3  Education              : int  2 1 4 5 1 3 2 3 3 4 ...
#  C>3  EducationField         : chr  "Life Sciences" "Life Sciences" "Other" "Life Sciences" ...
#  C=2  Gender                 : chr  "Female" "Female" "Male" "Male" ...
# C>3  JobLevel               : int  1 1 4 3 1 4 2 2 3 4 ...
# C>3  JobRole                : chr  "Healthcare Representative" "Research Scientist" "Sales Executive" "Human Resources" ...
# C=3  MaritalStatus          : chr  "Married" "Single" "Married" "Married" ...
# Cont MonthlyIncome          : int  131160 41890 193280 83210 23420 40710 58130 31430 20440 134640 ...
# C>3  NumCompaniesWorked     : int  1 0 1 3 4 3 2 2 0 1 ...
# Cont PercentSalaryHike      : int  11 23 15 11 12 13 20 22 21 13 ...
# c>3  StockOptionLevel       : int  0 1 3 3 2 0 1 3 0 1 ...
# Cont TotalWorkingYears      : int  1 6 5 13 9 28 5 10 10 6 ...
# Cont TrainingTimesLastYear  : int  6 3 2 5 2 5 2 2 2 2 ...
# Cont YearsAtCompany         : int  1 5 5 8 6 7 0 0 9 6 ...
# Cont YearsSinceLastPromotion: int  0 1 0 7 0 7 0 0 7 1 ...
# Cont YearsWithCurrManager   : int  0 4 3 5 4 7 0 0 8 5 ...
# c>3  JobInvolvement         : int  3 2 3 2 3 3 3 3 3 3 ...
# c=2  PerformanceRating      : int  3 4 3 3 3 3 4 4 4 3 ...
# C>3 (with one NA) EnvironmentSatisfaction: int  3 3 2 4 4 3 1 1 2 2 ...
# c>3 (with one NA) JobSatisfaction        : int  4 2 2 4 1 2 3 2 4 1 ...
# c>3 (with one NA) WorkLifeBalance        : int  2 4 1 3 3 2 1 3 3 3 ...
# Cont mean_work_hours        : num  6.55 6.98 6.5 6.48 7.52 ...
# c=2  extra_working_hrs      : num  0 0 0 0 1 1 0 0 0 0 ...
# cont no_of_leaves           : int  18 14 8 15 5 13 18 7 20 16 ...

################################################################################################################################
################  Checking duplicates ,NA, blanks ##########################################################################################
################################################################################################################################
sum(duplicated(hr_data))
#0

sum(is.na(hr_data)) # quite less as compared to complete data , so omitting NA
hr_data <- na.omit(hr_data) #Removing NA's

#Checking for individual fields
sapply(hr_data, function(x) length(which(is.na(x)))) #No Na found

sapply(hr_data, function(x) length(which(x == ""))) #No blank spaces 

#############################################################################################################################################
############################ Distribution of Data ###########################################################################################
################################################################################################################################
par(mfrow = c(4,2))
par(mar = rep(2,4))

hist(hr_data$Age)
hist(hr_data$DistanceFromHome)
hist(hr_data$MonthlyIncome)
hist(hr_data$NumCompaniesWorked)
hist(hr_data$PercentSalaryHike)
hist(hr_data$TrainingTimesLastYear)
hist(hr_data$YearsAtCompany)
hist(hr_data$YearsSinceLastPromotion)


###################### Univariate Analysis #################################
## User-defined Function for Univariate Analysis
################################# #########################################
univariate_analysis <- function(df,col){
  types<-df[,col]
  df %>% ggplot(aes(x=types))+ geom_bar(aes(fill=types)) + labs(x=tolower(col),y="No of Employees")
}


univariate_analysis(hr_data,"EducationField")
univariate_analysis(hr_data,"MaritalStatus")
univariate_analysis(hr_data,"JobRole")
univariate_analysis(hr_data,"Gender")
univariate_analysis(hr_data,"Department")
univariate_analysis(hr_data,"BusinessTravel")
univariate_analysis(hr_data,"Attrition")

univariate_analysis(hr_data,"DistanceFromHome")
univariate_analysis(hr_data,"MonthlyIncome")
univariate_analysis(hr_data,"NumCompaniesWorked")
univariate_analysis(hr_data,"PercentSalaryHike")
univariate_analysis(hr_data,"TrainingTimesLastYear")
univariate_analysis(hr_data,"Age")
univariate_analysis(hr_data,"YearsAtCompany")
univariate_analysis(hr_data,"YearsSinceLastPromotion")

hr_agg <- hr_data %>% group_by(Attrition) %>% summarise(cnt = n())
label_text <- round(100 * hr_agg$cnt/nrow(hr_data),2)
plot_attrition <- ggplot(hr_agg,aes(x=Attrition,y=cnt,fill=Attrition)) + geom_bar(stat="identity") +
  geom_text(aes(label=paste0(label_text,'%')),vjust = 2)
plot_attrition

#Conclusion : About 16.16% of employees left the company in the year 2015

#################################################################################################################
####################### Bivariate Analysis ####################################
#################################################################################################################
bar_theme<- theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.1), 
                   legend.position="none")

plot_grid(ggplot(hr_data, aes(x=BusinessTravel,fill=Attrition))+ geom_bar()+bar_theme,
          ggplot(hr_data, aes(x=Department,fill=Attrition))+ geom_bar() +bar_theme,
          ggplot(hr_data, aes(x=EducationField,fill=Attrition))+ geom_bar()+bar_theme,
          ggplot(hr_data, aes(x=Gender,fill=Attrition))+ geom_bar()+bar_theme,
          ggplot(hr_data, aes(x=JobLevel,fill=Attrition))+ geom_bar()+bar_theme,
          ggplot(hr_data, aes(x=MaritalStatus,fill=Attrition))+ geom_bar()+bar_theme,
          align = "v")   

#################################################################################################################
#Conclusions from the chart:
#Those who travelled rarely tend to leave company more.
#Attrition is less for HR dept. as low proportion of HR employees are there in an organization. Also Research & 
#Development Dept. have high attrition 
#In correlation with Department, HR have less attrition if considering education field
#Attrition rate is more among male employees as we can assume compared to female employees they tend to have more
#dependants which results in seeking a good package
#Assuming Joblevel 1 denotes employees who have just entered the role/field and 5 denoting highly experienced or 
#experts of the field. From the plot we understand that as the level increases people are less tend to leave 
#company
#Attrition is more among singles and less among divorcees
##################################################################################################################

plot_grid(ggplot(hr_data, aes(x=StockOptionLevel,fill=Attrition))+ geom_bar() +bar_theme,
          ggplot(hr_data, aes(x=EnvironmentSatisfaction,fill=Attrition))+ geom_bar()+bar_theme,
          ggplot(hr_data, aes(x=JobSatisfaction,fill=Attrition))+ geom_bar()+bar_theme,
          ggplot(hr_data, aes(x=WorkLifeBalance,fill=Attrition))+ geom_bar()+bar_theme,
          ggplot(hr_data, aes(x=JobInvolvement,fill=Attrition))+ geom_bar()+bar_theme,
          ggplot(hr_data, aes(x= factor(PerformanceRating),fill=Attrition))+ geom_bar()+bar_theme
          +labs(x="PerformanceRating"),
          align = "v") 
##################################################################################################################
#Conclusions from the chart:
#StockOptionLevel - More employees quit in lower levels
#EnvironmentSatisfaction - More employees quit where the statisfaction level is 1 compared to other levels. But not
#showing any significant trend
#JobSatisfaction - Higher attrition among Job Satisfaction level 1.
#WorkLifeBalance - if considering the proportion of data, attrition is more in level 1 which is as expected.
#JobInvolvement  - Attrition is more with involvement level 3
#PerformanceRating - compared to rating 4, rating 3 has high attrition amount.
##################################################################################################################

# Boxplots of numeric variables relative to Attrition 
plot_grid(ggplot(hr_data, aes(x=Attrition,y=MonthlyIncome, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(hr_data, aes(x=Attrition,y=PercentSalaryHike, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() ,
          ggplot(hr_data, aes(x=Attrition,y=YearsSinceLastPromotion, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() ,
          align = "v",nrow = 1)

######################################################################################### 
################################ CORRELATION MATRIX #####################################
######################################################################################### 

correlation_matrix <- function(data){
  cordata <- round(cor(data),2)
  head(cordata) #This will be a 5X5 matrix with each correlation values
  
  #Melt data to bring the correlation values in two axis
  melted_cordata <- melt(cordata)
  head(melted_cordata)
  plot <- ggplot(data = melted_cordata, aes(x=Var1, y=Var2, fill=value, 
                                            label= value))
  plot_tile <- plot + geom_tile()
  plot_fill_color <- plot_tile + scale_fill_gradient2(low = "#132B43",high ="#56B1F7"
                                                      ,mid = "white")
  plot_label <- plot_fill_color + geom_text()
  plot_label_box <- plot_label + geom_label()
  plot_label
}

data<- hr_data[, c("Age","YearsWithCurrManager","mean_work_hours","no_of_leaves", "DistanceFromHome", "MonthlyIncome", "NumCompaniesWorked", "PercentSalaryHike", "TotalWorkingYears", "YearsAtCompany", "YearsSinceLastPromotion")]

correlation_matrix(data)

################################################################################################################################
#################################### Outlier Treatment #############################################################################################
################################################################################################################################
print("Any value, which is beyond the range of -1.5 x IQR to 1.5 x IQR is treated as outlier")

treatOutlier<-function(df,col){
  max_limit<-as.numeric(quantile(df[,col],na.rm=T)[4] + 1.5*IQR(df[,col],na.rm = T))
  min_limit<-as.numeric(quantile(df[,col],na.rm=T)[2] - 1.5*IQR(df[,col],na.rm=T))
  df[,col][which(df[,col]> max_limit)]<-max_limit
  df[,col][which(df[,col]< min_limit)]<-min_limit
  
  print(paste0("Outliers handled for " , col))
  return(df)
}
continuous_vars=c("DistanceFromHome","Age","MonthlyIncome","NumCompaniesWorked","PercentSalaryHike",
                  "TotalWorkingYears","TrainingTimesLastYear","YearsAtCompany","YearsSinceLastPromotion",
                  "YearsWithCurrManager","mean_work_hours","no_of_leaves")

########## Calling traetOutlier function for continuous variables #############
for( var in continuous_vars){
  hr_data<-treatOutlier(hr_data,var)
}


sapply(hr_data[,c("DistanceFromHome","Age","MonthlyIncome","NumCompaniesWorked","PercentSalaryHike",
                  "TotalWorkingYears","TrainingTimesLastYear","YearsAtCompany","YearsSinceLastPromotion",
                  "YearsWithCurrManager","mean_work_hours","no_of_leaves")], 
       function(x) quantile(x,seq(0,1,.01),na.rm = T))

################################################################################################################################
######################## Normalising continuous features ########################################################################################
######################## ######################## ########################################################################################

hr_data$DistanceFromHome<- scale(hr_data$DistanceFromHome) 
hr_data$Age<- scale(hr_data$Age)
hr_data$MonthlyIncome<- scale(hr_data$MonthlyIncome)
hr_data$NumCompaniesWorked<- scale(hr_data$NumCompaniesWorked)
hr_data$PercentSalaryHike<- scale(hr_data$PercentSalaryHike)
hr_data$TotalWorkingYears<- scale(hr_data$TotalWorkingYears)
hr_data$TrainingTimesLastYear<- scale(hr_data$TrainingTimesLastYear)
hr_data$YearsAtCompany<- scale(hr_data$YearsAtCompany)
hr_data$YearsSinceLastPromotion<- scale(hr_data$YearsSinceLastPromotion)
hr_data$YearsWithCurrManager<- scale(hr_data$YearsWithCurrManager)
hr_data$mean_work_hours<- scale(hr_data$mean_work_hours)
hr_data$no_of_leaves<- scale(hr_data$no_of_leaves)

#REMOVING employee id from the data set as it is not required for analysis
hr_data<-hr_data[,-c(1)]

###############################################################################################
######################## DUMMY VARIABLES ################################################ 
################################################################################################ 
#### Creating a dataframe of categorical variables
hr_chr<- hr_data[,-c(1,2,5,8,12,13,14,16,17,18,19,20,26,27,28)]

# converting categorical attributes to factor
hr_fact<- data.frame(sapply(hr_chr, function(x) factor(x)))

# creating dummy variables for factor attributes
dummies<- data.frame(sapply(hr_fact, 
                            function(x) data.frame(model.matrix(~x-1,data =hr_fact))[,-1]))

hr_final<- cbind(hr_data[,c(1,2,5,8,12,13,14,16,17,18,19,20,26,27,28)],dummies) 

#Converting target variable to 0,1
hr_final$Attrition<- ifelse(hr_final$Attrition=="Yes",1,0)
hr_final$Gender<- ifelse(hr_final$Gender=="Female",1,0)

################################################################################################ 
######################## BUILDING MODEL ################################################ 
################################################################################################ 
#break the data into test and train
set.seed(100)
trainindices<-sample(1:nrow(hr_final), 0.7*nrow(hr_final))

train <- hr_final[trainindices,]
test <- hr_final[-trainindices,]

##################################################################################
##################### Start Model building #########################################
##################################################################################

######### Logistic Regression: 

#Initial model
model_1 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1) 


# Stepwise selection
library("MASS")
model_2<- stepAIC(model_1, direction="both")


# Removing multicollinearity through VIF check
summary(model_2)

library(car)
vif(model_2)

#Developing new models till we get robust model
model_3<- glm(formula =Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                extra_working_hrs + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                Department.xResearch...Development + Department.xSales + 
                Education.x3 + Education.x4 + Education.x5 + EducationField.xOther + 
                EducationField.xTechnical.Degree + JobLevel.x2 + JobLevel.x5 + 
                JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                JobRole.xSales.Executive + MaritalStatus.xSingle + StockOptionLevel.x1 + 
                JobInvolvement.x3 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4, family = "binomial", data = train)

summary(model_3)
vif(model_3)


##############################################################################

#Remove BusinessTravel.xTravel_Rarely as high VIf and Insignificant
model_4<- glm(formula =Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                extra_working_hrs + BusinessTravel.xTravel_Frequently +  + 
                Department.xResearch...Development + Department.xSales + 
                Education.x3 + Education.x4 + Education.x5 + EducationField.xOther + 
                EducationField.xTechnical.Degree + JobLevel.x2 + JobLevel.x5 + 
                JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                JobRole.xSales.Executive + MaritalStatus.xSingle + StockOptionLevel.x1 + 
                JobInvolvement.x3 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4, family = "binomial", data = train)
summary(model_4)
vif(model_4)

## Now since All the high VIfs have high significance too ,
## So , start removing the insignificant variables

#Remove Education.x3 as found most insignificant among others in model
model_5<- glm(formula =Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                extra_working_hrs + BusinessTravel.xTravel_Frequently +  + 
                Department.xResearch...Development + Department.xSales + 
                Education.x4 + Education.x5 + EducationField.xOther + 
                EducationField.xTechnical.Degree + JobLevel.x2 + JobLevel.x5 + 
                JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                JobRole.xSales.Executive + MaritalStatus.xSingle + StockOptionLevel.x1 + 
                JobInvolvement.x3 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4, family = "binomial", data = train)

summary(model_5)
vif(model_5)

#Remove Education.x4  as found most insignificant among others in model
model_6<-  glm(formula =Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 extra_working_hrs + BusinessTravel.xTravel_Frequently +  + 
                 Department.xResearch...Development + Department.xSales + 
                 Education.x5 + EducationField.xOther + 
                 EducationField.xTechnical.Degree + JobLevel.x2 + JobLevel.x5 + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xSingle + StockOptionLevel.x1 + 
                 JobInvolvement.x3 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4, family = "binomial", data = train)
summary(model_6)
vif(model_6)

#Remove Education.x5    as found most insignificant among others in model
model_7<- glm(formula =Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                extra_working_hrs + BusinessTravel.xTravel_Frequently +  + 
                Department.xResearch...Development + Department.xSales + 
                EducationField.xOther + 
                EducationField.xTechnical.Degree + JobLevel.x2 + JobLevel.x5 + 
                JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                JobRole.xSales.Executive + MaritalStatus.xSingle + StockOptionLevel.x1 + 
                JobInvolvement.x3 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4, family = "binomial", data = train)


summary(model_7)
vif(model_7)

#Remove StockOptionLevel.x1    as found most insignificant among others in model
model_8<- glm(formula =Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                extra_working_hrs + BusinessTravel.xTravel_Frequently +  + 
                Department.xResearch...Development + Department.xSales + 
                EducationField.xOther + 
                EducationField.xTechnical.Degree + JobLevel.x2 + JobLevel.x5 + 
                JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                JobRole.xSales.Executive + MaritalStatus.xSingle  + 
                JobInvolvement.x3 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4, family = "binomial", data = train)


summary(model_8)
vif(model_8)

#Remove EducationField.xOther     as found most insignificant among others in model
model_9<- glm(formula =Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                extra_working_hrs + BusinessTravel.xTravel_Frequently +  + 
                Department.xResearch...Development + Department.xSales + 
                EducationField.xTechnical.Degree + JobLevel.x2 + JobLevel.x5 + 
                JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                JobRole.xSales.Executive + MaritalStatus.xSingle  + 
                JobInvolvement.x3 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4, family = "binomial", data = train)


summary(model_9)
vif(model_9)

#Remove EducationField.xTechnical.Degree  as found most insignificant among others in model
model_10<-glm(formula =Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                extra_working_hrs + BusinessTravel.xTravel_Frequently +  + 
                Department.xResearch...Development + Department.xSales + 
                JobLevel.x2 + JobLevel.x5 + 
                JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                JobRole.xSales.Executive + MaritalStatus.xSingle  + 
                JobInvolvement.x3 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4, family = "binomial", data = train)

summary(model_10)
vif(model_10)

#Remove JobLevel.x5    as found most insignificant among others in model
model_11<-  glm(formula =Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  extra_working_hrs + BusinessTravel.xTravel_Frequently +  + 
                  Department.xResearch...Development + Department.xSales + 
                  JobLevel.x2  + 
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + MaritalStatus.xSingle  + 
                  JobInvolvement.x3 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4, family = "binomial", data = train)

summary(model_11)
vif(model_11)

#Remove JobRole.xResearch.Director  as found most insignificant among others in model
model_12<-  glm(formula =Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  extra_working_hrs + BusinessTravel.xTravel_Frequently +  + 
                  Department.xResearch...Development + Department.xSales + 
                  JobLevel.x2  + 
                  JobRole.xManufacturing.Director  + 
                  JobRole.xSales.Executive + MaritalStatus.xSingle  + 
                  JobInvolvement.x3 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4, family = "binomial", data = train)

summary(model_12)
vif(model_12)


#Remove JobRole.xSales.Executive   as found most insignificant among others in model
model_13<-  glm(formula =Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  extra_working_hrs + BusinessTravel.xTravel_Frequently +  + 
                  Department.xResearch...Development + Department.xSales + 
                  JobLevel.x2  + 
                  JobRole.xManufacturing.Director  + 
                  MaritalStatus.xSingle  + 
                  JobInvolvement.x3 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4, family = "binomial", data = train)
summary(model_13)
vif(model_13)

#Remove Department.xSales     as found most insignificant among others in model
model_14<-glm(formula =Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                extra_working_hrs + BusinessTravel.xTravel_Frequently +  + 
                Department.xResearch...Development  + 
                JobLevel.x2  + 
                JobRole.xManufacturing.Director  + 
                MaritalStatus.xSingle  + 
                JobInvolvement.x3 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4, family = "binomial", data = train)

summary(model_14)
vif(model_14)

#Remove Department.xResearch...Development     as found most insignificant among others in model
model_15<-glm(formula =Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                extra_working_hrs + BusinessTravel.xTravel_Frequently +  + 
                JobLevel.x2  + 
                JobRole.xManufacturing.Director  + 
                MaritalStatus.xSingle  + 
                JobInvolvement.x3 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4, family = "binomial", data = train)


summary(model_15)
vif(model_15)


#Remove PercentSalaryHike     as found most insignificant among others in model
model_16<- glm(formula =Attrition ~ Age + NumCompaniesWorked  + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 extra_working_hrs + BusinessTravel.xTravel_Frequently +  + 
                 JobLevel.x2  + 
                 JobRole.xManufacturing.Director  + 
                 MaritalStatus.xSingle  + 
                 JobInvolvement.x3 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4, family = "binomial", data = train)

summary(model_16)
vif(model_16)

#Remove JobInvolvement.x3   as found most insignificant among others in model
model_17<-glm(formula =Attrition ~ Age + NumCompaniesWorked  + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                extra_working_hrs + BusinessTravel.xTravel_Frequently +  + 
                JobLevel.x2  + 
                JobRole.xManufacturing.Director  + 
                MaritalStatus.xSingle  + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4, family = "binomial", data = train)
summary(model_17)
vif(model_17)

#Remove JobLevel.x2     as found most insignificant among others in model
model_18<- glm(formula =Attrition ~ Age + NumCompaniesWorked  + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 extra_working_hrs + BusinessTravel.xTravel_Frequently +  + 
                 JobRole.xManufacturing.Director  + 
                 MaritalStatus.xSingle  + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4, family = "binomial", data = train)

summary(model_18)
vif(model_18)


#Remove JobSatisfaction.x2     as found most insignificant among others in model
model_19<- glm(formula =Attrition ~ Age + NumCompaniesWorked  + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 extra_working_hrs + BusinessTravel.xTravel_Frequently +  + 
                 JobRole.xManufacturing.Director  + 
                 MaritalStatus.xSingle  + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4  + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4, family = "binomial", data = train)

summary(model_19)
vif(model_19)

#Remove JobSatisfaction.x3     as found most insignificant among others in model
model_20<-glm(formula =Attrition ~ Age + NumCompaniesWorked  + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                extra_working_hrs + BusinessTravel.xTravel_Frequently +  + 
                JobRole.xManufacturing.Director  + 
                MaritalStatus.xSingle  + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4   + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4, family = "binomial", data = train)

summary(model_20)
vif(model_20)

#Remove JobRole.xManufacturing.Director     as found most insignificant among others in model
model_21<-glm(formula =Attrition ~ Age + NumCompaniesWorked  + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                extra_working_hrs + BusinessTravel.xTravel_Frequently +  + 
                MaritalStatus.xSingle  + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4   + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4, family = "binomial", data = train)

summary(model_21)
vif(model_21)

#Remove TrainingTimesLastYear    as found most insignificant among others in model
model_22<- glm(formula =Attrition ~ Age + NumCompaniesWorked  + TotalWorkingYears + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 extra_working_hrs + BusinessTravel.xTravel_Frequently +  + 
                 MaritalStatus.xSingle  + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4   + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4, family = "binomial", data = train)

summary(model_22)
vif(model_22)

#Remove WorkLifeBalance.x4   as found most insignificant among others in model
model_23<-  glm(formula =Attrition ~ Age + NumCompaniesWorked  + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  extra_working_hrs + BusinessTravel.xTravel_Frequently +  + 
                  MaritalStatus.xSingle  + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4   + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 
                  , family = "binomial", data = train)

summary(model_23)
vif(model_23)



#Remove WorkLifeBalance.x2   as found most insignificant among others in model
model_24<-    glm(formula =Attrition ~ Age + NumCompaniesWorked  + TotalWorkingYears + 
                    YearsSinceLastPromotion + YearsWithCurrManager + 
                    extra_working_hrs + BusinessTravel.xTravel_Frequently +  + 
                    MaritalStatus.xSingle  + 
                    EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                    EnvironmentSatisfaction.x4   + 
                    JobSatisfaction.x4  + WorkLifeBalance.x3 
                  , family = "binomial", data = train)
summary(model_24)
vif(model_24)


#Remove Age       as found most insignificant among others in model
model_25<-   glm(formula =Attrition ~  NumCompaniesWorked  + TotalWorkingYears + 
                   YearsSinceLastPromotion + YearsWithCurrManager + 
                   extra_working_hrs + BusinessTravel.xTravel_Frequently +  + 
                   MaritalStatus.xSingle  + 
                   EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                   EnvironmentSatisfaction.x4   + 
                   JobSatisfaction.x4  + WorkLifeBalance.x3 
                 , family = "binomial", data = train)
summary(model_25)
vif(model_25)

#Remove EnvironmentSatisfaction.x2 as found most insignificant among others in model
model_26<-   glm(formula =Attrition ~  NumCompaniesWorked  + TotalWorkingYears + 
                   YearsSinceLastPromotion + YearsWithCurrManager + 
                   extra_working_hrs + BusinessTravel.xTravel_Frequently +  + 
                   MaritalStatus.xSingle  + 
                   EnvironmentSatisfaction.x3 + 
                   EnvironmentSatisfaction.x4   + 
                   JobSatisfaction.x4  + WorkLifeBalance.x3 
                 , family = "binomial", data = train)
summary(model_26)
vif(model_26)

#Remove EnvironmentSatisfaction.x3 as found most insignificant among others in model
model_27<-   glm(formula =Attrition ~  NumCompaniesWorked  + TotalWorkingYears + 
                   YearsSinceLastPromotion + YearsWithCurrManager + 
                   extra_working_hrs + BusinessTravel.xTravel_Frequently +  + 
                   MaritalStatus.xSingle  + 
                   EnvironmentSatisfaction.x4   + 
                   JobSatisfaction.x4  + WorkLifeBalance.x3 
                 , family = "binomial", data = train)
summary(model_27)
vif(model_27)

#Remove YearsWithCurrManager as found most insignificant among others in model
model_28<-   glm(formula =Attrition ~  NumCompaniesWorked  + TotalWorkingYears + 
                   YearsSinceLastPromotion + YearsWithCurrManager + 
                   extra_working_hrs + BusinessTravel.xTravel_Frequently +  + 
                   MaritalStatus.xSingle  + 
                   JobSatisfaction.x4  + WorkLifeBalance.x3 
                 , family = "binomial", data = train)
summary(model_28)
vif(model_28)
# Final Model
#We can consider below  as final model model_28 with 9 variables
final_model<- model_28
#######################################################################

### Model Evaluation

### Test Data ####


#predicted probabilities of Attrition for test data
test_pred = predict(final_model, type = "response",newdata = test)

test$prob <- test_pred

# Let's use the probability cutoff of 50%.

test_pred_attr <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_attr <- factor(ifelse(test$Attrition==1,"Yes","No"))
table(test_actual_attr,test_pred_attr)
##               test_pred_attr
##test_actual_attr  No Yes
##             No  1055 15
##            Yes  173  47

test_pred_attr <- factor(ifelse(test_pred >= 0.30, "Yes", "No"))
test_actual_attr <- factor(ifelse(test$Attrition==1,"Yes","No"))
table(test_actual_attr,test_pred_attr)
##               test_pred_attr
##test_actual_attr  No Yes
##             No  968 102
##            Yes  109  111

test_pred_attr <- factor(ifelse(test_pred >= 0.25, "Yes", "No"))
test_actual_attr <- factor(ifelse(test$Attrition==1,"Yes","No"))

table(test_actual_attr,test_pred_attr)
##               test_pred_attr
##test_actual_attr  No Yes
##             No  904 166
##            Yes  90 130
#################################################
#ConfusionMatrix

library(caret)
perform_fn <- function(cutoff) 
{
  predicted_attr <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- caret ::: confusionMatrix(predicted_attr, test_actual_attr, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}
#######################################################################
perform_fn(0.50)
#sensitivity specificity  accuracy
# 0.2136364   0.9859813 0.8542636
perform_fn(0.40)
#sensitivity specificity  accuracy
# 0.3454545   0.9654206 0.8596899

perform_fn(0.30)
#sensitivity specificity accuracy
# 0.5045455   0.9046729 0.8364341

perform_fn(0.25)
#sensitivity specificity  accuracy
# 0.5909091   0.8448598 0.8015504

perform_fn(0.20)
#sensitivity specificity  accuracy
# 0.7045455   0.7841121 0.7705426

summary(test_pred)

s = seq(.01,.80,length=100)

s
OUT = matrix(0,100,3)

OUT

for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 
#########################################################################################
# Let's Choose the cutoff value. 
par(mfrow = c(1,1))
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.02)]
cutoff[2]
test_cutoff_attr <- factor(ifelse(test_pred >=0.1855556, "Yes", "No"))

conf_final <- caret ::: confusionMatrix(test_cutoff_attr, test_actual_attr, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc
#Accuracy 
#  0.7573643

sens
#Sensitivity 
#  0.7409091 

spec
#Specificity 
#  0.7607477

test_pred_attr <- factor(ifelse(test_pred >= 0.185556, "Yes", "No"))
test_actual_attr <- factor(ifelse(test$Attrition==1,"Yes","No"))

table(test_actual_attr,test_pred_attr)
##               test_pred_attr
##test_actual_attr  No Yes
##             No  814 256
##            Yes  57 163
#################################################
#ConfusionMatrix
perform_fn(0.185556)

##################################################################################################
### KS -statistic - Test Data ######

test_cutoff_attr <- ifelse(test_cutoff_attr=="Yes",1,0)
test_actual_attr <- ifelse(test_actual_attr=="Yes",1,0)


library(ROCR)
#on testing  data
pred_object_test<- prediction(test_cutoff_attr, test_actual_attr)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)
#0.5016568
####################################################################
# Lift & Gain Chart 

# plotting the lift chart

# Loading dplyr package 
require(dplyr)
library(dplyr)

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

library(bindrcpp)
Attrition_decile = lift(test_actual_attr, test_pred, groups = 10)
Attrition_decile

#################################################################################################################
#plot the lift chart 
################################################################################################################
library(scales)
plot(Attrition_decile$Cumlift, type="l", lwd=2, col="red",
     xlim = c(0,10),
     ylim = c(0,4),
     main = "Lift Chart",
     xlab = "Decile",
     ylab = "Lift")
abline(h=0.5, col="blue")
axis(1, 1:10)
abline(h=0:10, v=0:10, lty=3)
#################################################################################################################
#Plot Gain Chart 
################################################################################################################

## Installing package here as it is causing conflict with caret for some function like confusionMatrix
#install.packages("InformationValue")
library(InformationValue)
ks_plot(test_actual_attr, test_cutoff_attr) # Gain chart plot

#remove.packages("InformationValue")
#####################END###############################
################################################################################################################
#Conclusion:
#The Number of companies worked by a person is more they tend to quit more
#If an employee works with the same manager for a longer period of time the lesser are the chances that 
#employee will leave the company.
#As the experience increases the people are less tend to quit, which shows more comfortable with environment
#People with more experience as they are less likely to leave the company. 
#Job Satisfaction,work life balance are some of the main features that need to be taken for retaining
#employees
#The more an employee works extra work hours on an average the more are the chances that he/she will leave
#the company.
#Employees who are unmarried are prone to leaving the company.
################################################################################################################

