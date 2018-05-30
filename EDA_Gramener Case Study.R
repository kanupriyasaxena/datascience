########################## Setting the working directory ########################

########################## Loading the packages required ########################
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(reshape2)
library(corrplot)
########################## Data Sourcing ########################################
loan <- read.csv("loan.csv")

########################## Data Understanding ####################################
## Getting the glimpse of data 
glimpse(loan)

## Knowing first few(10) values in the columns
head(loan,10)

############################ Knowing the data  ###################################
# Having taken the glipse of data,you could find different types of variables such as 
# 1. Variables related to customers information
# 2. Variables related to the loan information
# 3. Variables related to the customers credit history.

## Segregating the variables on different types

############################ 1. Customer information variables ###################

#emp_title ,emp_length, home_ownership, annual_inc, verification_status, addr_state,
# zip_code, title, purpose, desc, url

############################ 2. Loan Information variables ########################

# loan amount, funded amount, funded amount invested, interest rate, loan status
# loan grade, loan sub-grade, dti, loan issue date, loan term, installment

############################ 3. Customer transactional Beahviour variables ########

#  delinq_2yrs,  earliest_cr_line,  inq_last_6mths,  open_acc,  pub_rec,  revol_bal
#  revol_util ,  total_acc, out_prncp ,  out_prncp_inv, total_pymnt, total_pymnt_inv
# total_rec_prncp, total_rec_int , total_rec_late_fee , recoveries, collection_recovery_fee
# last_pymnt_d, last_pymnt_amnt, next_pymnt_d, last_credit_pull_d, application_type,

############################# Business understanding ############################ 

# The company wants to understand the driving factors behind the loan default. 
# If one is able to identify these risky loan applicants,
# then such loans can be reduced thereby cutting down the amount of credit loss. 
# Identification of such applicants using EDA is the aim of this case study. 

############################  ############################ 
# Now,think of mapping business problem with the dataset.You could find that the variables related to the customer
# behaviour data cann't be collected at the time of applicantion. Thus analysing these variable could not solve 
# our business problem. In general,our company wants to understand the driving
# factors behind the loan default at the time of application stage. 

############################# Data cleaning ############################

############################# 1.Fix rows and columns ############################

# Removing the extra variables from the analysis

transaction_vars <-c("earliest_cr_line","inq_last_6mths","open_acc",
                     "pub_rec","revol_bal","revol_util","total_acc","out_prncp",
                     "out_prncp_inv","total_pymnt","total_pymnt_inv","total_rec_prncp",
                     "total_rec_int","total_rec_late_fee","recoveries",
                     "collection_recovery_fee","last_pymnt_d","mths_since_last_record",
                     "last_pymnt_amnt","next_pymnt_d",
                     "last_credit_pull_d","application_type")

## Removing the customer transactional behaviour variables from the analysis. 
loan <- loan[,!(colnames(loan) %in% transaction_vars)]


## Removing the columns having only NA values
loan<-loan[colSums(!is.na(loan)) > 0]

##Removing the columns having 2 unique values including 0/NA
loan<-loan[,!sapply(loan, function(x) length(unique(x))==2 & 
                      sum(unique(x) %in% c(0,NA))==2)]

##Removing the columns having only sigle unique value
loan<-loan[,!sapply(loan, function(x) length(unique(x))==1)]

## Getting the glimpse of data 
glimpse(loan)

## Removing the not useful columns like url,desc,emp_title,title,zip_code
#and pub_rec_bankruptcies which has most of the values as 0

loan<-loan[,!(colnames(loan) %in% c("url","desc","emp_title","title","zip_code","pub_rec_bankruptcies"))]

##Fixing invalid values
loan <- loan %>% mutate(term = gsub(" months", "", term),
                        term = gsub(" ", "", term),
                        int_rate = gsub("%", "", int_rate),
                        emp_length = gsub("years", "", emp_length),
                        emp_length = gsub("year", "", emp_length),
                        emp_length = gsub("\\+", "", emp_length),
                        emp_length = gsub("<", "", emp_length))

##Fixing the format of columns
loan <- loan %>% mutate(int_rate = as.numeric(int_rate),
                        term = as.numeric(term),
                        emp_length = as.numeric(emp_length),
                        term = as.factor(term),
                        issue_d = paste("01",issue_d,sep="-"),
                        issue_d = parse_date_time(issue_d, orders = c("dmy"), locale = "eng"),
                        loan_status = as.factor(loan_status),
                        purpose = as.factor(purpose),
                        annual_inc = as.double(loan$annual_inc))

##Checking for the duplicate request id data
sum(duplicated(loan$id))
sum(duplicated(loan$member_id))

##Checking for the NA values
sum(is.na(loan$id))
sum(is.na(loan$member_id))
sum(is.na(loan$term))
sum(is.na(loan$loan_status))
sum(is.na(loan$verification_status))
sum(is.na(loan$home_ownership))
sum(is.na(loan$annual_inc))

######################### Derived Metrics #######################################

#1. Deriving the Year , out of issue date

loan$issue_year <- as.factor(format(loan$issue_d,"%Y"))

#2. Creatng bins for Loan_amnt
loan$loan_amnt_bin<-ifelse(loan$loan_amnt<=5000,"Small",
                           ifelse(loan$loan_amnt>5000 & loan$loan_amnt<=15000,"Medium",
                                  ifelse(loan$loan_amnt>15000 & loan$loan_amnt<=25000,"High","VeryHigh")))

#3. Creatng bins for interest rate

loan$int_rate_bin<-ifelse(loan$int_rate<=10,"Low_rate",
                          ifelse(loan$int_rate>10 & loan$int_rate<=15,"Medium_rate","High_rate"))

#4. Creatng bins for dti

loan$dti_bin<-ifelse(loan$dti<=10,"Low_dti",
                     ifelse(loan$dti>10 & loan$dti<=20,"Medium_dti","High_dti"))

#5. Creatng bins for Funded amount
loan$funded_amnt_bin<-ifelse(loan$funded_amnt<=5000,"Small",
                             ifelse(loan$funded_amnt>5000 & loan$funded_amnt<=15000,"Medium",
                                    ifelse(loan$funded_amnt>15000 & loan$funded_amnt<=25000,"High","VeryHigh")))

#6. Creatng bins for Annual income
loan$annual_inc_bin<-ifelse(loan$annual_inc<=50000,"Small",
                            ifelse(loan$annual_inc>50000 & loan$annual_inc<=100000,"Medium",
                                   ifelse(loan$annual_inc>100000 & loan$annual_inc<=150000,"High","VeryHigh")))
#7. Creatng bins for Installment
loan$installment_bin<-ifelse(loan$installment<=200,"Small",
                             ifelse(loan$installment>200 & loan$installment<=400,"Medium",
                                    ifelse(loan$installment>400 & loan$installment<=600,"High","VeryHigh")))


########################### Segmented Data Preparation ############################
## Building 2 data frames
## 1. containing "Charged off Loans" and "Fully Paid" loans 
#  2. containing only Charged off Loans for analysis

loan_non_current <- loan %>% filter(loan_status != "Current")
loan_chargedOff <- loan %>% filter(loan_status %in% "Charged Off")
loan_paid <- loan %>% filter(loan_status %in% "Fully Paid")
########################### Data Analysis ########################################

##################### UNivariate Analysis ( Categorical Variables ################

##################### 1. Analysis on the basis of term ###########################

G_TE<-ggplot(loan_non_current %>% 
               group_by(term,loan_status) %>% 
               summarize(cnt=length(id))%>%
               group_by(loan_status) %>% mutate(per=paste0(round(100*cnt/sum(cnt),2),'%')), 
             aes(x= loan_status,fill=term)) + 
  geom_col(aes(y=cnt),position="dodge") +
  geom_text(stat='count',aes(label =per),position = position_dodge(.9),vjust=-.5)+
  labs(x = "Loan Status", y=" Count")+
  ggtitle("Status - Term wise analysis")

G_TE
##################### 2. Analysis on the basis of Annual Income ######################

## Analysing the data contianed in Annual inc column
summary(loan_chargedOff$annual_inc)

## Checking the quantile information
quantile(loan_chargedOff$annual_inc)[1:4]
quantile(loan_chargedOff$annual_inc)[4]

## As max limit pertains to 75% + 1.5 IQR where IQR is Q3-Q1 
max_limit<-as.numeric(quantile(loan_non_current$annual_inc)[4] + 1.5*IQR(loan_non_current$annual_inc))

G_AI<-ggplot(loan_chargedOff %>%filter(annual_inc <= max_limit) ,
             aes(x= annual_inc)) + 
  geom_histogram(aes(x=annual_inc,fill=loan_status),bins=50,position = "dodge")+
  labs(title="Annual Income Wise Analysis",x="Annual Income",y="Count")
G_AI
###################### 3. Analysis on the basis of home_ownership #################

G_HO<-ggplot(loan_chargedOff %>% 
               group_by(home_ownership,loan_status) %>% 
               summarize(cnt=length(id))%>%
               group_by(loan_status) %>% mutate(per=paste0(round(100*cnt/sum(cnt),2),'%')), 
             aes(x= home_ownership)) + facet_wrap(~loan_status)+
  geom_col(aes(y=cnt,fill=home_ownership)) +
  geom_text(stat='count',aes(label =per),vjust=0.8)+
  labs(x = "Home Ownership", y=" Count")+
  ggtitle("Home Ownership wise analysis")
G_HO
###################### 4. Analysis on the basis of purpose ###################### 

purpose_df<-loan_chargedOff %>% 
  group_by(purpose,loan_status) %>% 
  summarize(cnt=length(id))%>%
  group_by(loan_status) %>% mutate(per=round(100*cnt/sum(cnt),2))

## Taking top 5 purpose for analysis
purpose_df<-purpose_df[order(-purpose_df$per),][1:5,]
purpose_df$per<-paste0(purpose_df$per,'%')
G_PU<-ggplot(purpose_df, 
             aes(x=purpose,fill= purpose)) +
  geom_col(aes(y=cnt,fill=purpose),position = "dodge") +
  geom_text(stat='count',aes(label =per),position=position_dodge(1.0),vjust=0.9)+
  labs(x = "Purpose", y=" Count")+
  ggtitle("Purpose wise analysis")

G_PU
######################## 5. Analysis on the basis of verification status  #########

G_VS<-ggplot(loan_chargedOff %>% 
               group_by(verification_status,loan_status) %>% 
               summarize(cnt=length(id))%>%
               group_by(loan_status) %>% mutate(per=paste0(round(100*cnt/sum(cnt),2),'%')), 
             aes(x= verification_status,fill=verification_status)) + 
  geom_col(aes(y=cnt),position="dodge") +
  geom_text(stat='count',aes(label =per),position = position_dodge(.9),vjust=-.5)+
  labs(x = "Verification Status", y=" Count")+
  ggtitle(" Verification Status wise Analysis")

G_VS
######################### 6.Analysis on the basis of Grade  ######################## 

G_GR<-ggplot(loan_chargedOff,aes(x = as.factor(grade))) +
  geom_bar(aes(y = (..count..)/sum(..count..)),fill="Orange") +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  labs(title = "Grade Wise Analysis", y = "Percent", x = "Grade")+theme(
    axis.text.y=element_blank(), axis.ticks=element_blank(),
    axis.title.y=element_blank(),axis.text.x = element_text(angle = 60, hjust = 1)
  ) 
G_GR
######################### 7.Analysis on the basis of Sub-Grade  ######################## 
G_SG<- ggplot(loan_chargedOff %>% 
                group_by(sub_grade,loan_status) %>% 
                summarize(cnt=length(id)), 
              aes(x= sub_grade,fill=loan_status)) + 
  geom_col(aes(y=cnt,fill=loan_status)) +
  labs(x = "Sub Grade", y=" Count")+
  ggtitle("Sub Grade wise analysis")

G_SG
######################### 8.Analysis on the basis of Address State  ######################## 

G_AS<-ggplot(loan_chargedOff %>% 
               group_by(addr_state,loan_status) %>%
               summarize(cnt=length(id))) + 
  geom_col(aes(x=addr_state,y=cnt,fill=loan_status), position="dodge") + 
  labs(title="State - Loan Status",x="State",y="Count",fill="Loan Status")

G_AS

######################### 9.Analysis on the basis of Loan Amount  ######################## 

G_LA<-ggplot(loan_chargedOff,aes(x= loan_amnt,fill=loan_status)) + 
  geom_histogram(aes(x=loan_amnt,fill=loan_status),bins=1000) +
  labs(title="Loan Amount",x="Loan Amount",y="Count",fill="Loan Status")

G_LA
######################### 10.Analysis on the basis of Installment Amount  ######################## 

G_INS<-ggplot(loan_chargedOff,aes(x= installment,fill=loan_status)) + 
  geom_histogram(aes(x=installment,fill=loan_status),bins=10) +
  labs(title="Installment",x="Installment",y="Count",fill="Loan Status")

G_INS
######################### 10.Analysis on the basis of Delinq_2years   ######################## 

G_DE2<-ggplot(loan_chargedOff,aes(x= delinq_2yrs,fill=loan_status)) + 
  geom_histogram(aes(x=delinq_2yrs,fill=loan_status)) +
  labs(title="Delinq_2 years Analysis",x="No of Delinq",y="Count",fill="Loan Status")

G_DE2
######################### 11.Analysis on the basis of months since last delinq  ######################## 

G_MO<-ggplot(loan_chargedOff,aes(x= mths_since_last_delinq,fill=loan_status)) + 
  geom_histogram(aes(x=mths_since_last_delinq,fill=loan_status),bins=10, col="black") +
  labs(title="Months Since Last Delinq ",x="Months Since Last Delinq",y="Count",fill="Loan Status")

G_MO
######################### 11.Analysis on the basis of dti  ######################## 


G_DTI<-ggplot(loan_chargedOff,aes(x= dti,fill=loan_status)) + 
  geom_histogram(aes(x=dti,fill=loan_status),col="black") +
  labs(title="dti ",x="dti",y="Count",fill="Loan Status")


G_DTI
######################### 12.Analysis on the basis of employment length######################## 


G_EL<-ggplot(loan_chargedOff %>% 
               group_by(emp_length,loan_status) %>%
               summarize(cnt=length(id))) + 
  geom_col(aes(x=emp_length,y=cnt,fill=loan_status), position="dodge") + 
  labs(title="Charged Off count-Employment Length",x="Emp Length",y="Count",fill="Loan Status")


G_EL
######################### 13.Analysis on the basis of Year of Issue######################## 
G_YEAR<-loan_non_current %>% ggplot(aes(x = as.factor(issue_year))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  labs(title = "issue_year", y = "Percent", x = "term")+theme(
    axis.text.y=element_blank(), axis.ticks=element_blank(),
    axis.title.y=element_blank(),axis.text.x = element_text(angle = 30, hjust = 1)
  )

G_YEAR
# Till the univariate analysis, We have seen the distribution of the each and every variables of loan dataset.
# 57% of charged off applicants applied laon for 36 months term period
# We saw that loan applicants are increasing year on year, more than 50% of loan applicants received loans in 2011
# 25% of charged off applicants loans comes under grade B 
# Interest rate increases as a grade level increases from A to B
# 51 % of applicants are living in rented home whereas 41% applicants were mortagaged their home.  
# Most importantly, 49% of applicants applied loan for paying their other loans. 

##################################################################################################
# The univariate analysis signifies that the purpose of the loan is seen as 
# promising factor of impact
##################################################################################################

######################## Univariate Segmented  Analysis ############################################

## Analysing various factors on the basis of Purpose of Loan

## The pur(6.17%)pose of taking for the defaulters include
## 1. Debt_Consolidation(49.17%)
## 2. Credit_card (9.63%)
## 3. Other(11.25%)
## 4. Small_business (8.44%)
## 5. Home Improvement

## Lets take the top 3 categories of purpose for analysis excluding the OTHER

loan_chargedOff <- subset(loan_chargedOff,purpose %in% c("debt_consolidation","credit_card","small_business"))

#1. Based on top 3 purpose of loan , analyse term
loan_chargedOff %>% ggplot(aes(x = as.factor(term))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  labs(title = "term", y = "Percent", x = "term")+theme(
    axis.text.y=element_blank(), axis.ticks=element_blank(),
    axis.title.y=element_blank(),axis.text.x = element_text(angle = 30, hjust = 1)
  )+facet_wrap(~purpose)


#2. Based on purpose, analyse grade
loan_chargedOff %>% ggplot(aes(x = as.factor(grade))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  #scale_y_continuous(labels = percent) +
  labs(title = "grade", y = "Percent", x = "grade")+theme(
    axis.text.y=element_blank(), axis.ticks=element_blank(),
    axis.title.y=element_blank(),axis.text.x = element_text(angle = 30, hjust = 1)
  )+facet_wrap(~purpose)


#3. Based on purpose, analyse homeownership
loan_chargedOff %>% ggplot(aes(x = as.factor(home_ownership))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  #scale_y_continuous(labels = percent) +
  labs(title = "homeownership", y = "Percent", x = "homeownership")+theme(
    axis.text.y=element_blank(), axis.ticks=element_blank(),
    axis.title.y=element_blank(),axis.text.x = element_text(angle = 30, hjust = 1)
  )+facet_wrap(~purpose)

#4. Based on purpose, analyse verification status
loan_chargedOff %>% ggplot(aes(x = as.factor(verification_status))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  #scale_y_continuous(labels = percent) +
  labs(title = "verification_status", y = "Percent", x = "verification_status")+theme(
    axis.text.y=element_blank(), axis.ticks=element_blank(),
    axis.title.y=element_blank(),axis.text.x = element_text(angle = 30, hjust = 1)
  )+facet_wrap(~purpose)

#5. Based on purpose, analyse lOAN_AMNT
loan_chargedOff %>% ggplot(aes(x=loan_amnt)) + 
  geom_line(stat = 'density',color='red')+
  facet_wrap(~purpose)+ggtitle("loan_amnt")+xlab("loan_amnt")

#6. Based on purpose, analyse Interest rate
loan_chargedOff %>% ggplot(aes(x=int_rate)) + 
  geom_line(stat = 'density',color='red')+
  facet_wrap(~purpose)+ggtitle("int_rate")+xlab("int_rate")

#7. Based on purpose, analyse Annual Income
loan_chargedOff %>% ggplot(aes(x=annual_inc)) + 
  geom_line(stat = 'density',color='red')+
  facet_wrap(~purpose)+ggtitle("annual_inc")+xlab("annual_inc")

#8.  Based on purpose, analyse dti
loan_chargedOff %>% ggplot(aes(x=dti)) + 
  geom_line(stat = 'density',color='red')+
  facet_wrap(~purpose)+ggtitle("dti")+xlab("dti")


## Analysing various factors on the basis of Home Ownership

## The impacting Home Ownership include 
## 1. RENT(50%)
## 2.Mortgage(41%)

## Lets take the top 2 categories of Home Ownership for analysis excluding the OTHER

loan_chargedOff<-subset(loan_chargedOff,home_ownership %in% c("RENT","MORTGAGE"))
############ On the basis of Home Ownership


#1. Based on top 2 types of Home Ownership , analyse term
loan_chargedOff %>% ggplot(aes(x = as.factor(term))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  #scale_y_continuous(labels = percent) +
  labs(title = "term", y = "Percent", x = "term")+theme(
    axis.text.y=element_blank(), axis.ticks=element_blank(),
    axis.title.y=element_blank(),axis.text.x = element_text(angle = 30, hjust = 1)
  )+facet_wrap(~home_ownership)


#2.Based on top 2 types of Home Ownership, analyse grade
loan_chargedOff %>% ggplot(aes(x = as.factor(grade))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  #scale_y_continuous(labels = percent) +
  labs(title = "grade", y = "Percent", x = "grade")+theme(
    axis.text.y=element_blank(), axis.ticks=element_blank(),
    axis.title.y=element_blank(),axis.text.x = element_text(angle = 30, hjust = 1)
  )+facet_wrap(~home_ownership)

#3. Based on top 2 types of Home Ownership, analyse verification status
loan_chargedOff %>% ggplot(aes(x = as.factor(verification_status))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  #scale_y_continuous(labels = percent) +
  labs(title = "verification_status", y = "Percent", x = "verification_status")+theme(
    axis.text.y=element_blank(), axis.ticks=element_blank(),
    axis.title.y=element_blank(),axis.text.x = element_text(angle = 30, hjust = 1)
  )+facet_wrap(~home_ownership)

#4. Based on top 2 types of Home Ownership, analyse lOAN_AMNT
loan_chargedOff %>% ggplot(aes(x=loan_amnt)) + 
  geom_line(stat = 'density',color='red')+
  facet_wrap(~home_ownership)+ggtitle("loan_amnt")+xlab("loan_amnt")

#5. Based on top 2 types of Home Ownership, analyse lOAN_AMNT
loan_chargedOff %>% ggplot(aes(x=installment)) + 
  geom_line(stat = 'density',color='red')+
  facet_wrap(~home_ownership)+ggtitle("installment")+xlab("installment")

#6. Based on purpose, analyse Interest rate
loan_chargedOff %>% ggplot(aes(x=int_rate)) + 
  geom_line(stat = 'density',color='red')+
  facet_wrap(~home_ownership)+ggtitle("int_rate")+xlab("int_rate")

#7. Based on top 2 types of Home Ownership, analyse Annual Income
loan_chargedOff %>% ggplot(aes(x=annual_inc)) + 
  geom_line(stat = 'density',color='red')+
  facet_wrap(~home_ownership)+ggtitle("annual_inc")+xlab("annual_inc")

#8.  Based on purpose, analyse dti
loan_chargedOff %>% ggplot(aes(x=dti)) + 
  geom_line(stat = 'density',color='red')+
  facet_wrap(~home_ownership)+ggtitle("dti")+xlab("dti")
loan_non_current$home_ownership<-as.factor(loan_non_current$home_ownership)

########################### BIVariate Analysis ##############################################  

##########################Correlation Matrix ###############################
## Creating the corrrelation Matrix between the continuous numeric variables
str(loan_non_current)
mydata <- loan_non_current[, c("loan_amnt", "int_rate", "installment", "sub_grade", "annual_inc","dti","revol_util","delinq_2yrs")]
mydata <- loan_non_current[, c(3, 4, 5, 7,8, 13,19,20)]
cormat <- round(cor(mydata),2)
head(cormat)

melted_cormat <- melt(cormat)
head(melted_cormat)
corrplot(cormat, method = "number", 
         title = "Correlation Map ", 
         type = "lower", 
         order = "FPC", 
         number.cex = 0.9, 
         tl.cex = 1.0,
         bg="Red")
###################### 1.loan_amnt vs funded_amnt ################################

ggplot(loan_chargedOff,aes(x=loan_amnt,y=funded_amnt)) +
  geom_bin2d()+facet_wrap(~purpose)

ggplot(loan_chargedOff,aes(x=loan_amnt,y=funded_amnt)) + 
  geom_bin2d()+facet_wrap(~home_ownership)

cor(loan_chargedOff$loan_amnt, loan_chargedOff$funded_amnt)
#0.98

######################### 2.loan_amnt vs funded_amnt_inv################################
ggplot(loan_chargedOff,aes(x=funded_amnt,y=funded_amnt_inv)) + 
  geom_bin2d() + facet_wrap(~purpose)

ggplot(loan_chargedOff,aes(x=funded_amnt,y=funded_amnt_inv)) + 
  geom_bin2d() + facet_wrap(~home_ownership)

cor(loan_chargedOff$loan_amnt, loan_chargedOff$funded_amnt_inv)
#0.91

########################### 3. loan_amnt vs int_rate  ################################
ggplot(loan_chargedOff,aes(x=loan_amnt,y=int_rate)) + geom_smooth()


########################### 8.Grade vs Purpose  ################################
## Writing the results to excel sheet for clear analysis
uni_grade_purpose <- 
  loan_chargedOff %>% 
  group_by(grade,purpose) %>% 
  summarise(frequency=n())

ugp_wide<-spread(uni_grade_purpose,key=grade, value = frequency)
write.csv(ugp_wide,file="uni_grade_purpose.csv")

########################### 9. Grade vs sub_grade ################################
## Writing the results to excel sheet for clear analysis
uni_grade_subgrade <- 
  loan_chargedOff %>% 
  group_by(grade,sub_grade) %>% 
  summarise(frequency=n())

ugsg_wide<-spread(uni_grade_subgrade,key=grade, value = frequency)
write.csv(ugsg_wide,file="uni_grade_subgrade.csv")

####################### 10. Grade Vs loan_amount ################################
## Writing the results to excel sheet for clear analysis
uni_grade_loan_amnt <- 
  loan_chargedOff %>% 
  group_by(grade,loan_bin_5000=loan_amnt %/% 5000) %>% 
  summarise(frequency=n())

ugla_wide<-spread(uni_grade_loan_amnt,key=grade, value = frequency)
write.csv(ugla_wide,file="uni_grade_loan_amnt.csv")

###################### 11. interest Vs loan amount ################################
## Writing the results to excel sheet for clear analysis
uni_int_loan_amnt <- 
  loan_chargedOff %>% 
  group_by(int_rate_bin_3=int_rate %/%3,loan_bin_5000=loan_amnt %/% 5000) %>% 
  summarise(frequency=n())

uila_wide<-spread(uni_int_loan_amnt,key=int_rate_bin_3, value = frequency)
write.csv(uila_wide,file="uni_int_loan_amnt.csv")


###################### MULTIVARIATE ANALYSIS ########################################
################### 12. Int rate vs home ownership analysis#########################
ggplot(loan_paid,aes(x=(int_rate),fill = home_ownership)) + geom_histogram()+
  labs(x="Interest_rate",y="Count")+ggtitle("Home Ownership Wise Interest Rate Analysis for Fully Paid customers")
ggplot(loan_chargedOff,aes(x=(int_rate),fill = home_ownership)) + geom_histogram()+
  labs(x="Interest_rate",y="Count")+ggtitle("Home Ownership Wise Interest Rate Analysis for Charged Off customers")

ggplot(loan_non_current,aes(x=(int_rate),fill = home_ownership)) + geom_histogram()+
  facet_wrap(~loan_status)+
  labs(x="Interest_rate",y="Count")+ggtitle("Home Ownership Wise Interest Rate Analysis ")
## "people paying rent and Mortgage are highly skewed towards right in charged Off figure, which means that people paying 
## High interest rates and living on Rent OR Mortgage  are again under tight scrutiny #
## because they are very likely to default."



################### 13. Int rate vs Term analysis######################### 
ggplot(loan_chargedOff,aes(x=(int_rate),fill = term)) + geom_histogram()+
  labs(x="Interest_rate",y="Count")+ggtitle("Term Wise Interest Rate Analysis for Charged Off customers")
ggplot(loan_paid,aes(x=(int_rate),fill = term)) + geom_histogram()+
  labs(x="Interest_rate",y="Count")+ggtitle("Term Wise Interest Rate Analysis for Fully Paid customers")

ggplot(loan_non_current,aes(x=(int_rate),fill = term)) + geom_histogram()+
  facet_wrap(~loan_status)+
  labs(x="Interest_rate",y="Count")+ggtitle("Term Wise Interest Rate Analysis for Charged Off customers")


##"with this graph it becomes a little clear that interest rates are generally low for fully paid ones, So higher interest rate may 
##drive people to default ( not surprising).Also, in conjunction with TERM there is evidence that most people with 
##LESS interest rates and 36 month time period will pay there loan , but on other hand people with HIGH interest rates 
##and getting loan for 60 month time period are more likely to default.

##The people in the middle (with interest rates lying in median range) can go anywhere. BUT even then if they are taking 
##loan for 60 month period , there are hich chances they may default, because in second curve (charged OFF) there are 
##quite a lot of people with 60 months lying in middle and not paying loans."

################### 14. LoanAmnt/Annual Inc ######################## 


ggplot(loan_paid,aes(x=(loan_amnt/annual_inc),fill = home_ownership)) + 
  geom_histogram(breaks=seq(0,0.75,by=0.015))+
  labs(x="loan_amnt/annual_inc",y="Count")+ggtitle("Charged Off customers")

ggplot(loan_chargedOff,aes(x=(loan_amnt/annual_inc),fill = home_ownership)) + 
  geom_histogram(breaks=seq(0,0.75,by=0.015))+
  labs(x="loan_amnt/annual_inc",y="Count")+ggtitle("Fully Paid customers")

ggplot(loan_non_current,aes(x=(loan_amnt/annual_inc),fill = home_ownership)) + 
  geom_histogram(breaks=seq(0,0.75,by=0.015))+
  facet_wrap(~loan_status)+
  labs(x="loan_amnt/annual_inc",y="Count")+ggtitle("Charged Off customers")

##"ratio of loan amount / annual income which gives loan amount as % of annual income

##There is something unusual about people on mortgage asking for loan greater than 15%. They end up being more defaulters.
##In the first one , charged OFF, there are unusual spikes in data and the mortgage color width is unusually wider in 
##charged off especiallly after 0.15 range.
##So anyone paying mortgage , and asking for loan above 15% oh his annual income is subject to higher scrutiny"

#################################### Thank You #####################

