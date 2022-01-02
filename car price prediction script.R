library("ggplot2")    #Create Elegant Data Visualizations 
library("moments")    #Moments, skewness, kurtosis and related tests
library("car")  #Required for Calculations Related to Regression
library("corrplot")   #Visualization of Correlation Matrix

data1 = read.csv("E:/Data Science course/TOSEE/s12/CS_03.CSV", header = TRUE )
dim(data1)
View(head(data1))

str(data1)

summary(data1)

#"Price"    : Sales price in Euro     
#"Age"      : Age of a used car in month 
#"KM"       : Kilometerage
#"FuelType" : Petrol, Diesel, CNG
#"HP"       : Horse power     
#"MetColor" : 1 : if Metallic color, 0 : Not
#"Automatic": 1 : if Aoutomatic, 0 : Not
#"CC"       : Engine displacement in cc
#"Doors"    : # of doors
#"Weight"   : Weight in Kilogram

#conver categorical vars to factor  


cat_vars = c("FuelType", "MetColor", "Automatic", "Doors")
data1[,cat_vars] = lapply(data1[,cat_vars] , factor)
str(data1)

summary(data1)


#numerical vars distribution 
par(mfrow = c(2,3)) 
for(i in c(1,2,3,5,8,10)){
  hist(data1[,i], xlab = "" , main = paste("histogram of", names(data1)[i]))
}
par(mfrow = c(1,1))


#correlation analysis
cor_table = round(cor(data1[,c(1,2,3,5,8,10)]),2)
cor_table
corrplot(cor_table)

#scatter plot 
par(mfrow = c(2,3))
for(i in  c(1,2,3,5,8,10)){
  plot(data1[,i] , data1$Price, xlab = "", ylab = "price", main = paste("price versus", names(data1)[i]))
}
par(mfrow = c(1,1))
#categorical vars 
table(data1$FuelType)
table(data1$MetColor)
table(data1$Automatic)
table(data1$Doors)
#tip: small samples for cng cars and 2-door cars

#divide dataset into test and train sets
#train 
set.seed(1234)
train_cases = sample( 1: nrow(data1) , 0.7*nrow(data1))
train = data1[train_cases,]
summary(train)
dim(train)

#test 
test = data1[-train_cases,]
summary(test)
str(test)
dim(test)

#start modeling 
#model1 
m1 = lm(Price ~ KM , data = train)

summary(m1)

#chek assumptions of reg 
#normality of residuals
hist(m1$residuals , probability = TRUE)
lines(density(m1$residuals) , col = "red" , lwd = 3 )

#QQ plot 

qqnorm(m1$residuals ,main = "QQ-plot of residuals", pch = 20)
qqline(m1$residuals , col = "red")

#test for skewness and kurtosis
#Good for sample size > 25
#Jarque-Bera Test (Skewness = 0 ?)
jarque.test(m1$residuals)


#Anscombe-Glynn Test (Kurtosis = 3 ?)
anscombe.test(m1$residuals)

#tip: resuduals are not normally distributed

#diagnostic plots
plot(m1)


cooks.distance(m1)
sum(cooks.distance(m1)>1)

#Note: Residuals are not Normally Distributed!
#Note: Hetroscedasticity Problem!
plot(data1$Price , data1$KM)

#model 2
#quadratic regression 
m2 = lm(Price~ KM + I(KM^2)  , data = train)
summary(m2)

#check assumptions of regression 
#normality
hist(m2$residuals , probability = TRUE , breaks = 20)
lines(density(m2$residuals) , col = "red")

#QQ plot
qqnorm(m2$residuals)
qqline(m2$residuals , col = "red")

#normality tests 
#skewness
jarque.test(m2$residuals)

#kurtosis
anscombe.test(m2$residuals)

#note = residuals are not normally distributed 

#diagnostic plots 
plot(m2)

#linear vs quadratic regression 
ggplot(train , aes(x = KM , y = Price)) + 
  geom_point() +
  geom_smooth(method = "lm" , formula = y ~ x , col = "red") + 
  geom_smooth(method = "lm" , formula = y ~ x + I(x^2) , col = "purple") + 
  ggtitle("price vs km")


rem1 = c(10, 7 , 32 )
train2 = train[ - which(rownames(train)%in% rem1),]
dim(train2)

#model m2_2
m2_2 = lm(Price ~ KM + I(KM^2) , data = train2)
summary(m2_2)

hist(m2_2$residuals , probability = TRUE)
lines(density(m2_2$residuals) , col = "red" , lwd = 3)

qqnorm(m2_2$residuals)
qqline(m2_2$residuals , col = "red")

jarque.test(m2_2$residuals)

anscombe.test(m2_2$residuals)

plot(m2_2)

#note : deviation from normality assumption got improved 

coef(m2)
coef(m2_2)

#multicolinearity 

vif(m2)
vif(m2_2)
#If VIF > 10 then multicollinearity is high

train$KM_scaled = scale(train$KM)
head(train)

train2$KM_scaled = scale(train2$KM)

m2_3 = lm(Price ~ KM_scaled + I(KM_scaled^2) , data = train2)
summary(m2_3)

plot(m2_3)
car::vif(m2_3)

#model 3
colnames(train)
m3 = lm(Price ~ KM_scaled + I(KM_scaled^2) + Age + FuelType + HP+ MetColor + Automatic + 
          CC + Doors + Weight , data = train )
summary(m3)

#R-squared:  0.7746, Adjusted R-squared:  0.7714

plot(m3)

#Removing variables: HP, MetColor, and Doors 
cor(train$Price , train$HP)
plot(train$HP, train$Price)

tapply(train$Price , train$MetColor , mean) #there is not significant difference in prices 
boxplot(train$Price ~ train$MetColor)

tapply(train$Price , train$Doors , mean)
boxplot(train$Price ~ train$Doors)

m3 = lm(Price ~ KM + I(KM^2) +  Age + 
          FuelType + Automatic + CC + Weight, data = train)
summary(m3)
#R-squared:  0.7746, Adjusted R-squared:  0.7714

plot(m3)


#model 4
table(train$FuelType)
train$ifpetrol = ifelse(train$FuelType == "Petrol",TRUE,FALSE)
head(train)
table(train$ifpetrol)

m4 = lm(Price ~ KM + I(KM^2) +  Age + 
          ifpetrol + Automatic + CC + Weight, data = train)
summary(m4)

#Check Assumptions of Regression
#Normality of residuals
hist(m4$residuals , probability = TRUE , breaks = 20)
lines(density(m4$residuals) , col = "red")

qqnorm(m4$residuals)
qqline(m4$residuals , col = "red ")

jarque.test(m4$residuals)
anscombe.test(m4$residuals)

#Note: Residuals are not Normally Distributed!

plot(m4)

#model 4_2
train3 = train[- which(rownames(train) %in% c(112,491,82,83,284,293,114,803,948,544)),]

head(train3)
m4 = lm(Price ~ KM + I(KM^2) +  Age + 
          ifpetrol + Automatic + CC + Weight, data = train3)
summary(m4)

plot(m4)

hist(m4$residuals , probability = TRUE , breaks = 10)
lines(density(m4$residuals) , col = "red")

qqnorm(m4$residuals)
qqline(m4$residuals , col = "red")

jarque.test(m4$residuals)

anscombe.test(m4$residuals)

#remove at and cc 
m4_2 = lm(Price ~ KM_scaled + I(KM_scaled^2) +  Age + 
            ifpetrol + Weight, data = train3)
summary(m4_2)

hist(m4_2$residuals , probability = TRUE , breaks  = 20  )
lines(density(m4_2$residuals) , col = "red")

qqnorm(m4_2$residuals)
qqline(m4_2$residuals , col = "red")

jarque.test(m4_2$residuals)
anscombe.test(m4_2$residuals)
#note: H0 is not rejected normaliry assumption is accepted  

plot(m4_2)

#chek multicolinearity 
vif(m4_2)

#coefficients of the model 
coef(m4_2)

#confidence interval for coefficients 
confint(m4_2)

#number of removed cases 
dim(train)
dim(train3)
# 10 cases have been removed 
(927-918)/927 * 100
train

#test the model 
test$ifpetrol  = ifelse(test$FuelType == "Petrol" , TRUE , FALSE)
head(test)

test$KM_scaled = scale(test$KM)
head(test)


#prediction 
test$pred = predict(m4_2 , test)



#actual vs prediction 

plot(test$Price,test$pred , xlab = "Actual" , ylab = "Prediction")
abline(a = 0 , b= 1 , col = "red" )

#absolut err 
abs_err = abs(test$Price - test$pred)

#MAE
mean(abs_err)
#792

#Median
median(abs_err)
#607

#SD
sd(abs_err)
#746

#max
max(abs_err)
#8437

#min
min(abs_err)
#3.01

#histogram and  box plot
hist(abs_err , breaks = 20)

boxplot(abs_err)


#err percentage 
e_percent <- abs(test$Price - test$pred)/test$Price  * 100

#MAPE
mean(e_percent)
#8.3

median(e_percent)
#6.371448

max(e_percent)
#90.33035

min(e_percent)
#0.042388824


#Marketing Requirement
sum(e_percent <= 15)/ length(e_percent) * 100

