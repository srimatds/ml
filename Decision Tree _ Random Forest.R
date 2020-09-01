customer_churn<-read.csv("C:/Users/INTELLIPAAT/Desktop/customer_churn.csv")

library(caTools)
sample.split(customer_churn$Churn,SplitRatio = 0.65)-> split_tag
subset(customer_churn, split_tag==T)->train
subset(customer_churn, split_tag==F)->test

library(tree)

tree(Churn~tenure, data=train)-> mod_tree1
plot(mod_tree1)
text(mod_tree1)

tree(Churn~tenure+MonthlyCharges, data=train)-> mod_tree2
plot(mod_tree2)
text(mod_tree2)

tree(Churn~tenure+MonthlyCharges+Contract+TechSupport, data=train)-> mod_tree3
plot(mod_tree3)
text(mod_tree3)

predict(mod_tree1,newdata=test,type="class")->result1
head(result1)
table(test$Churn, result1)

predict(mod_tree2,newdata=test,type="class")->result2
head(result2)
table(test$Churn, result2)

predict(mod_tree3,newdata=test,type="class")->result3
head(result3)
table(test$Churn, result3)

-----------------------------------------------
  
library(caTools)  
sample.split(customer_churn$Churn,SplitRatio = 0.65)-> split_tag
subset(customer_churn, split_tag==T)->train
subset(customer_churn, split_tag==F)->test

library(randomForest)

randomForest(Churn~MonthlyCharges+tenure+gender+InternetService+Contract, data=train, mtry=3,ntree=100)-> mod_forest1

importance(mod_forest1)
varImpPlot(mod_forest1)
varImpPlot(mod_forest1, col="palegreen4")

predict(mod_forest1,newdata=test,type="class")->result_forest
head(result_forest)
table(test$Churn, result_forest)

randomForest(Churn~MonthlyCharges+tenure+gender+InternetService+Contract, data=train, mtry=4, ntree=100)-> mod_forest2

predict(mod_forest2,newdata=test,type="class")->result_forest2
head(result_forest2)
table(test$Churn, result_forest2)

randomForest(Churn~MonthlyCharges+tenure+gender+InternetService+Contract, data=train, mtry=5, ntree=100)-> mod_forest3
predict(mod_forest3,newdata=test,type="class")->result_forest3
head(result_forest3)
table(test$Churn, result_forest3)

randomForest(Churn~MonthlyCharges+tenure+gender+InternetService+Contract+OnlineSecurity+OnlineBackup+DeviceProtection+TechSupport, data=train, mtry=3, ntree=100)-> mod_forest4
importance(mod_forest4)
varImpPlot(mod_forest4, col="palegreen4")

predict(mod_forest4,newdata=test,type="class")->result_forest4
head(result_forest4)
table(test$Churn, result_forest4)

