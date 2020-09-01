customer_churn <- read.csv("/Users/abhishekdas/Desktop/Intellipat/Data_Science_Course_Documents/customer_churn.csv")

library(caTools)
library(ROCR)

#Split the data into train and test data sets
rows=seq(1,nrow(customer_churn))

trainRows=sample(rows,(70*nrow(customer_churn))/100)
train1 = customer_churn[trainRows,] 
test1 = customer_churn[-trainRows,]

#logistic regression model
glm(Churn~MonthlyCharges, data= customer_churn, family="binomial") ->log_mod1
summary(log_mod1)

predict(log_mod1,data.frame(MonthlyCharges=50),type="response")
predict(log_mod1,data.frame(MonthlyCharges=77),type="response")
predict(log_mod1,data.frame(MonthlyCharges=20:100),type="response")

glm(Churn~tenure, data= customer_churn, family="binomial") ->log_mod2
summary(log_mod2)

predict(log_mod2,data.frame(tenure=10),type="response")
predict(log_mod2,data.frame(tenure=70),type="response")
predict(log_mod2,data.frame(tenure=10:70),type="response")

#-----------------------------------------
#confusion matrix

#library(caTools)  

sample.split(customer_churn$Churn,SplitRatio = 0.65)-> split_tag
subset(customer_churn, split_tag==T)->train
subset(customer_churn, split_tag==F)->test

nrow(train)
nrow(test)

glm(Churn~MonthlyCharges, data=train, family = "binomial")-> mod_log

#summary
summary(mod_log)

#prediction on test

predict(mod_log,newdata=test,type="response")->result_log
head(result_log)
range(result_log)

table(test$Churn, result_log>0.3)
table(test$Churn, result_log>0.35)

#---------------------------------------------------------
#Multiple Logistic Regression
  
#library(caTools)  
sample.split(customer_churn$Churn,SplitRatio = 0.65)-> split_tag
subset(customer_churn, split_tag==T)->train
subset(customer_churn, split_tag==F)->test

nrow(train)
nrow(test)

glm(Churn~gender+Partner+InternetService+MonthlyCharges, data=train, family = "binomial")-> mod_log1

predict(mod_log1,newdata=test,type="response")->result_log1
head(result_log1)
range(result_log1)
table(test$Churn, result_log1>0.4)

#--

glm(Churn~PaymentMethod+TechSupport+tenure+PaperlessBilling, data=train, family = "binomial")-> mod_log2
predict(mod_log2,newdata=test,type="response")->result_log2
head(result_log2)

range(result_log2)
table(test$Churn, result_log2>0.4)

#--
  
glm(Churn~Contract+Dependents+MultipleLines+DeviceProtection, data=train, family = "binomial")-> mod_log3
predict(mod_log3,newdata=test,type="response")->result_log3
head(result_log3)

range(result_log3)
table(test$Churn, result_log3>0.4)

#-----------------------------------------------------------------------------------------
#ROCR


sample.split(customer_churn$Churn,SplitRatio = 0.65)-> split_tag
subset(customer_churn, split_tag==T)->train
subset(customer_churn, split_tag==F)->test

nrow(train)
nrow(test)

glm(Churn~MonthlyCharges, data=train, family = "binomial")-> mod_log
predict(mod_log,newdata=test,type="response")->result_log
head(result_log)
range(result_log)

table(test$Churn, result_log>0.1)

prediction(result_log,test$Churn) -> predict_log
performance(predict_log,"acc")->acc
plot(acc)

table(test$Churn, result_log>0.41)
performance(predict_log,"tpr","fpr") -> roc_curve
plot(roc_curve)
plot(roc_curve, colorize=T)

table(test$Churn,result_log>0.28)

performance(predict_log,"auc")->auc
auc

##############################################################


