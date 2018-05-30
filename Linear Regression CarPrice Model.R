library(MASS)
library(car)
library(tidyr)

#read the file
carPrice <- read.csv("CarPrice_Assignment.csv", stringsAsFactors = FALSE)

View(carPrice)
str(carPrice)

#####################Data preparation START########################

#seperate car name and car Price
carPrice<-separate(carPrice, CarName, c("carCompany", "carName"), sep = " ", remove = TRUE)
#removing car name column as it is not to be used for the analysis
carPrice<-carPrice[,-4]
#removing duplicate values
unique(carPrice)
#check Na values
sum(is.na(carPrice))
#No NA values found

str(carPrice)
summary(carPrice)

#carcompany seems to have a lot of value in Others group, so needs to be looked into
unique(carPrice$carCompany)

#spelling mismatches corrected
carPrice$carCompany[which(carPrice$carCompany=="nissan")] <- "Nissan"
carPrice$carCompany[which(carPrice$carCompany=="porcshce")] <- "porsche"
carPrice$carCompany[which(carPrice$carCompany=="toyouta")] <- "toyota"
carPrice$carCompany[which(carPrice$carCompany=="vokswagen")] <- "volkswagen"
carPrice$carCompany[which(carPrice$carCompany=="vw")] <- "volkswagen"
carPrice$carCompany[which(carPrice$carCompany=="maxda")] <- "mazda"

#fuel systems also seem to have some values in Others
unique(carPrice$fuelsystem)

#no treatment required for fuel system

#the categorical variables are
#symboling, car company, fuel type,aspiration,
#door number,car body,drive wheel, engine location,engine type,cylinder number,
# fuel system

#changing the categorical variables to dummy
#symboling
carPrice$symboling<- as.factor(carPrice$symboling)
dummy1<-model.matrix(~symboling - 1,data=carPrice)
dummy1<-dummy1[,-1]

#car company
carPrice$carCompany<- as.factor(carPrice$carCompany)
dummy2<-model.matrix(~carCompany - 1,data=carPrice)
dummy2<-dummy2[,-1]

#fuel type
carPrice$fueltype<- as.factor(carPrice$fueltype)
dummy3<-model.matrix(~fueltype -1,data=carPrice)
dummy3<-dummy3[,-1]

#aspiration
carPrice$aspiration<- as.factor(carPrice$aspiration)
dummy4<-model.matrix(~aspiration -1,data=carPrice)
dummy4<-dummy4[,-1]

#door number
carPrice$doornumber<- as.factor(carPrice$doornumber)
dummy5<-model.matrix(~doornumber -1,data=carPrice)
dummy5<-dummy5[,-1]

#car body
carPrice$carbody<- as.factor(carPrice$carbody)
dummy6<-model.matrix(~carbody -1,data=carPrice)
dummy6<-dummy6[,-1]

#drive wheel
carPrice$drivewheel<- as.factor(carPrice$drivewheel)
dummy7<-model.matrix(~drivewheel -1,data=carPrice)
dummy7<-dummy7[,-1]

#engine location
carPrice$enginelocation<- as.factor(carPrice$enginelocation)
dummy8<-model.matrix(~enginelocation -1,data=carPrice)
dummy8<-dummy8[,-1]

#engine type
carPrice$enginetype<- as.factor(carPrice$enginetype)
dummy9<-model.matrix(~enginetype -1,data=carPrice)
dummy9<-dummy9[,-1]

#cyliner number
carPrice$cylindernumber<- as.factor(carPrice$cylindernumber)
dummy10<-model.matrix(~cylindernumber -1,data=carPrice)
dummy10<-dummy10[,-1]

#fuel systems
carPrice$fuelsystem<- as.factor(carPrice$fuelsystem)
dummy11<-model.matrix(~fuelsystem -1,data=carPrice)
dummy11<-dummy11[,-1]

##bind all the dummy variables
carPrice_1<-cbind(carPrice[,-c(2,3,4,5,6,7,8,9,15,16,18)],dummy1,dummy2,dummy3,dummy4,dummy5,dummy6,dummy7,dummy8,dummy9,dummy10,dummy11)

##as Id will not be a variable, removing ID
carPrice_1<-carPrice_1[,-1]
unique(carPrice_1)

##check summary to see if any numeric variable needs treatment
summary(carPrice)
#####################Data preparation END########################


#####################Model building STARTS########################
#run the first model
model_1<-lm(price~.,data=carPrice_1)
summary(model_1)

#run step AIC for 1st model
step<-stepAIC(model_1, direction="both")
step



model_2<-lm(formula = price ~ wheelbase + carlength + carwidth + carheight + 
              curbweight + enginesize + boreratio  + compressionratio + 
              peakrpm + highwaympg  + carCompanybmw + 
              carCompanybuick + carCompanychevrolet + carCompanydodge + 
              carCompanyhonda + carCompanyisuzu + carCompanyjaguar + carCompanymazda + 
              carCompanymercury + carCompanymitsubishi + carCompanyNissan + 
              carCompanypeugeot + carCompanyplymouth + carCompanyrenault + 
              carCompanysubaru + carCompanytoyota + carCompanyvolkswagen + 
              carCompanyvolvo + dummy3 + dummy4 + carbodyhardtop + carbodyhatchback + 
              carbodysedan + carbodywagon + dummy8 + enginetypel + enginetypeohcv + 
              enginetyperotor + cylindernumberfive +  
              fuelsystem2bbl , data = carPrice_1)


summary(model_2)
vif(model_2)



model_3<-lm(formula = price ~ wheelbase + carlength + carwidth + carheight + 
              curbweight + enginesize + boreratio  + compressionratio + 
              peakrpm   + carCompanybmw + 
              carCompanybuick + carCompanychevrolet + carCompanydodge + 
              carCompanyhonda + carCompanyisuzu + carCompanyjaguar + carCompanymazda + 
              carCompanymercury + carCompanymitsubishi + carCompanyNissan + 
              carCompanypeugeot + carCompanyplymouth + carCompanyrenault + 
              carCompanysubaru + carCompanytoyota + carCompanyvolkswagen + 
              carCompanyvolvo + dummy3 + dummy4  + carbodyhatchback + 
               dummy8 + enginetypel + enginetypeohcv + 
              enginetyperotor + cylindernumberfive +  
              fuelsystem2bbl , data = carPrice_1)


summary(model_3)
vif(model_3)


model_4<-lm(formula = price ~ wheelbase + carlength + carwidth + carheight + 
              curbweight + enginesize + boreratio  + 
              peakrpm   + carCompanybmw + 
              carCompanybuick + carCompanychevrolet + carCompanydodge + 
              carCompanyhonda + carCompanyisuzu +  carCompanymazda + 
              carCompanymercury + carCompanymitsubishi + carCompanyNissan + 
              carCompanypeugeot + carCompanyplymouth + carCompanyrenault + 
              carCompanysubaru + carCompanytoyota + carCompanyvolkswagen + 
               dummy3 +  carbodyhatchback + 
              dummy8 + enginetypel + enginetypeohcv + 
              enginetyperotor + cylindernumberfive +  
              fuelsystem2bbl , data = carPrice_1)

summary(model_4)
vif(model_4)




model_5<-lm(formula = price ~ wheelbase + carlength + carwidth + carheight + 
              curbweight + enginesize + boreratio  + 
              peakrpm   + carCompanybmw + 
              carCompanybuick + carCompanychevrolet + carCompanydodge + 
              carCompanyhonda + carCompanyisuzu +  carCompanymazda + 
              carCompanymercury + carCompanymitsubishi + carCompanyNissan + 
              carCompanypeugeot + carCompanyplymouth + carCompanyrenault + 
              carCompanysubaru + carCompanytoyota + carCompanyvolkswagen + 
              dummy3 +  carbodyhatchback + 
              dummy8 + enginetypel + enginetypeohcv + 
              enginetyperotor + cylindernumberfive +  
              fuelsystem2bbl , data = carPrice_1)

summary(model_5)
vif(model_5)


model_6<-lm(formula = price ~  carlength + carwidth + 
              curbweight + enginesize + boreratio  + 
              peakrpm   + carCompanybmw + 
              carCompanybuick + carCompanychevrolet + carCompanydodge + 
              carCompanyhonda + carCompanyisuzu +  carCompanymazda + 
              carCompanymercury + carCompanymitsubishi + carCompanyNissan + 
              carCompanypeugeot + carCompanyplymouth + carCompanyrenault + 
              carCompanysubaru + carCompanytoyota + carCompanyvolkswagen + 
               carbodyhatchback + 
              dummy8 +  enginetypeohcv + 
               cylindernumberfive  , data = carPrice_1)

summary(model_6)
vif(model_6)


model_7<-lm(formula = price ~  carlength + carwidth + 
              curbweight + enginesize +  
              peakrpm   + carCompanybmw + 
              carCompanybuick + carCompanychevrolet + carCompanydodge + 
              carCompanyhonda + carCompanyisuzu +  carCompanymazda + 
              carCompanymercury + carCompanymitsubishi + carCompanyNissan + 
              carCompanypeugeot + carCompanyplymouth + carCompanyrenault + 
              carCompanysubaru + carCompanytoyota + carCompanyvolkswagen + 
              carbodyhatchback + 
              dummy8 +  enginetypeohcv + 
              cylindernumberfive  , data = carPrice_1)

summary(model_7)
vif(model_7)


model_8<-lm(formula = price ~ carlength + carwidth + curbweight + enginesize + 
              peakrpm + carCompanybmw + carCompanybuick + carCompanydodge + 
              carCompanyhonda +  carCompanymitsubishi + 
              carCompanyNissan + carCompanypeugeot + carCompanyplymouth + 
              carCompanyrenault + carCompanysubaru + carCompanytoyota + 
              carCompanyvolkswagen  + dummy8 + enginetypeohcv + 
              cylindernumberfive, data = carPrice_1)



summary(model_8)
vif(model_8)

#As all variables are significant in the above model
#but some VIFs are pretty hight
#length, width, cuberweight and enginesize can have good correlation
#lets check the correlation between each pair formed for them

cor(carPrice_1$carlength,carPrice_1$carwidth)
cor(carPrice_1$curbweight,carPrice_1$enginesize)
cor(carPrice_1$carlength,carPrice_1$enginesize)
cor(carPrice_1$carlength,carPrice_1$curbweight)
cor(carPrice_1$carwidth,carPrice_1$curbweight)
cor(carPrice_1$carwidth,carPrice_1$enginesize)
cor(carPrice_1$cylindernumberfive,carPrice_1$enginesize)

##length and width have a high correlation with cuberweight
##length and width are also appearing to be related 
## so building model 9 by removing width and cuberweight and retaining length


model_9<-lm(formula = price ~ carlength + enginesize + 
              peakrpm + carCompanybmw + carCompanybuick + carCompanydodge + 
              carCompanyhonda +  carCompanymitsubishi + 
              carCompanyNissan + carCompanypeugeot + carCompanyplymouth + 
              carCompanyrenault + carCompanysubaru + carCompanytoyota + 
              carCompanyvolkswagen  + dummy8 + enginetypeohcv + 
              cylindernumberfive, data = carPrice_1)



summary(model_9)
vif(model_9)



model_10<-lm(formula = price ~ carlength + enginesize + 
              peakrpm + carCompanybmw + carCompanybuick + carCompanydodge + 
              carCompanyhonda +  carCompanymitsubishi + 
              carCompanyNissan  + carCompanyplymouth + 
              carCompanyrenault + carCompanysubaru + carCompanytoyota + 
              carCompanyvolkswagen  + dummy8 , data = carPrice_1)



summary(model_10)
vif(model_10)

##All variables in model 10 are significant 
##But VIF is still high for length and engine size
##lets try removing engine size and see the impact on R2
## The R2 is still 77% which is good

model_11<-lm(formula = price ~ carlength + 
               peakrpm + carCompanybmw + carCompanybuick + carCompanydodge + 
               carCompanyhonda +  carCompanymitsubishi + 
               carCompanyNissan  + carCompanyplymouth + 
               carCompanyrenault + carCompanysubaru + carCompanytoyota + 
               carCompanyvolkswagen  + dummy8 , data = carPrice_1)



summary(model_11)
vif(model_11)


#####################Model building ENDS########################


#####################Model Testing STARTS########################

Predict<-predict(model_11, carPrice_1)
cor(carPrice_1$price, Predict)
(cor(carPrice_1$price, Predict))^2 

#####################Model Testing ENDS########################


#########Final Model############
#The model 11 is the final model chosen because of the following reasons
#All variables in this model are significant
#The R2 is 77% which is good
#The VIF of all the variables are in range <2
# The correlation between the predicted value and actual value of car price
#It has a value of 0.882
################################
