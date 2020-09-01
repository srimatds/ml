# Dependent = Monthly charges ; Indep = Tenure

#Read data
customer_churn<-read.csv("Customer_churn.csv")

#install.packages("caTools")

library(caTools)

#Split data into training and testing : ratio of 65:35

split_tag<-sample.split(customer_churn$MonthlyCharges,SplitRatio = 0.65)

train<-subset(customer_churn, split_tag==TRUE)

test<-subset(customer_churn, split_tag==FALSE)

nrow(train)
nrow(test)

#Plot data to see if there is relationship

plot(MonthlyCharges~tenure, data=train,col="red")

abline(lm(MonthlyCharges~tenure,data=train))

#Fit regresion model between Monthly charges and Tenure

model1 <- lm(MonthlyCharges~tenure, data=train)


summary(model1)


#Predict test data

predicted_values <- predict(model1, newdata=test)

#Bind Prections with actual values

final_data <- cbind(Actual=test$MonthlyCharges,Predicted=predicted_values)

head(final_data)

class(final_data)

as.data.frame(final_data)->final_data

class(final_data)

final_data$Actual - final_data$Predicted ->error
head(error)

cbind(final_data,error)-> final_data
head(final_data)

#Calculating root mean squared-error of the predictions

rmse1<-sqrt(mean((final_data$error)^2))


#Since RMSE is not great, we can conclude that this is not a great model

------------------------------------------------------------------------------------

#Linear Regression of Monthly charges with Internet Service
  
plot(MonthlyCharges~InternetService,data=train)
  
model2<-lm(MonthlyCharges~InternetService,data=train) 

summary(model2)

result<-predict(model2,newdata=test) 

final_data2<-cbind(Actual=test$MonthlyCharges,Predicted=result)
head(final_data2)

final_data2<-as.data.frame(final_data2)

error2<-(final_data2$Actual-final_data2$Predicted)

final_data2<-cbind(final_data2,error2)

rmse2<-sqrt(mean((final_data2$error2)^2))

summary(model2)

-----------------------------------------------------------------------------------------------

#Multiple Linear Regression

  
model3<-lm(MonthlyCharges~InternetService+tenure,data=train) 

summary(model3)

result<-predict(model3,newdata=test) 

final_data3<-cbind(Actual=test$MonthlyCharges,Predicted=result)
head(final_data3)

final_data3<-as.data.frame(final_data3)

error3<-(final_data3$Actual-final_data3$Predicted)

final_data3<-cbind(final_data3,error3)

rmse3<-sqrt(mean((final_data3$error3)^2))
  
-----------------------------------------------------------------------------------
  
library(caTools)  

sample.split(customer_churn$tenure,SplitRatio = 0.65)-> split_model
subset(customer_churn, split_model==T)->train
subset(customer_churn, split_model==F)->test

nrow(train)
nrow(test)

lm(tenure~MonthlyCharges+gender+InternetService+Contract, data=train)-> mod1
predict(mod1,test)->result1

cbind(Actual=test$tenure,Predicted=result1)->final_data1
head(final_data1)
class(final_data1)

as.data.frame(final_data1)->final_data1
class(final_data1)
final_data1$Actual - final_data1$Predicted ->error1
head(error1)
cbind(final_data1,error1)-> final_data1
head(final_data1)

sqrt(mean((final_data1$error1)^2))->rmse1
---
lm(tenure~Partner+PhoneService+TotalCharges+PaymentMethod,data=train)-> mod2
predict(mod2,test)-> result2
cbind(Actual=test$tenure,Predicted=result2)->final_data2
head(final_data2)

as.data.frame(final_data2)-> final_data2
(final_data2$Actual-final_data2$Predicted)->error2
head(error2)

cbind(final_data2,error2) -> final_data2
sqrt(mean((final_data2$error2)^2,na.rm=T))->rmse2

---------------------------------------------------
#Assumptions
  
library(ggplot2)  

ggplot(data= customer_churn, aes(x=tenure, y=TotalCharges)) + geom_point()
ggplot(data= customer_churn, aes(x=tenure, y=TotalCharges)) + geom_point()+geom_smooth(method = "lm")

lm(TotalCharges~tenure, data = customer_churn)->mod1
predict(mod1,data=customer_churn)-> result1
cbind(Actual=customer_churn$TotalCharges, Predicted=result1)-> final_data1

head(final_data1)
as.data.frame(final_data1)->final_data1
final_data1$Actual -final_data1$Predicted  -> error1
cbind(final_data1,error1)-> final_data1

ggplot(data= final_data1, aes(x=Predicted, y=error1)) + geom_point()

qqnorm(final_data1$error1)
