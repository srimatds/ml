# Clear Global environment
rm(list=ls())


library(C50)
library(rpart)
library(rpart.plot)
library(party)
library(randomForest)

#Predict flower species(classify)

iris
head(iris)
dim(iris)
names(iris) 
str(iris)
table(iris$Species)

#split train and test

s = sample(150,100)

iris_train = iris[s,]
iris_test = iris[-s,]

dim(iris_train)
dim(iris_test)

#################
#C5.0 good classification technique but does not provide good plots. For that rpart is good

Model_C50 <-C5.0(iris_train[,-5],iris_train[,5])
Model_C50
summary(Model_C50)

#Predicting on Train
P1_train=predict(Model_C50,iris_train)

P1_train
table(iris_train[,5],Predicted=P1_train)

#Predicting on Test

P1_test = predict(Model_C50,iris_test)
P1_test
table(iris_test[,5],Predicted=P1_test)

#################
#rpart
#################


Model_rpart= rpart(Species~.,data=iris_train, method="class")

#plotting rpart using ctree of party rpart(Species~.,data=iris_train, method="class") library
plot(Model_rpart, main="Classifcation Tree", margin=0.15, uniform=TRUE)
text(Model_rpart, use.n=T)

# Another visualization 
#plot(ctree(Species~.,data=iris_train))

Model_rpart
summary(Model_rpart)


#rpart.plot(Model_rpart,type=3)
rpart.plot(Model_rpart)
rpart.rules(Model_rpart)

#Predicting on Train
P1_train_rpart=predict(Model_rpart,iris_train,type="class")
table(iris_train[,5],predicted=P1_train_rpart)

#Predicting on Test
P1_test_rpart=predict(Model_rpart,iris_test,type="class")


#####

#Random Forest

random_forest <- randomForest(Species~.,data=iris_train, keep.forest=TRUE, ntree=101)

summary(random_forest)
print(random_forest)
random_forest$importance

varImpPlot(random_forest)

#predict in Train set
pred_model_train <-predict(random_forest,iris_train,type="response", norm.votes=TRUE)

#predict in test set
pred_model_test <-predict(random_forest,iris_test,type="response", norm.votes=TRUE)




