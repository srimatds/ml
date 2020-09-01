customer_churn <- read.csv("/Users/abhishekdas/Desktop/Intellipat/Data_Science_Course_Documents/customer_churn.csv")

#setwd("/Users/abhishekdas/Desktop/Intellipat/Data_Science_Course_Documents")

num1 <- c(1,2,3,4,5)
num2 <- c(10:20)
char1 <- c("a","b","c")
char2 <- c("this","is","sparta")
my_log1 <- c(TRUE,FALSE,TRUE,FALSE)
my_log2 <- c(T,F,T,F)

length(num1)
length(char1)
char2[1]
my_log2[c(1,3)]


a <- c(NA,6,7,8,NA,NA)
is.na(a) #whether any missing values are there in "a" or not
a <- ifelse(is.na(a),0,a)

#churn$new <- ifelse(churn$SeniorCitizen == 1, "yes","No")

my_list1<-list(1,"a",TRUE)
my_list2<-list(c(1,2),c("a","b"),c(TRUE,FALSE))

my_list1[[2]]
my_list2[[3]][2]

Fruit_list <- list(Apple = 85, Banana = 45, Guava = 100)
Fruit_list$Apple
#--------------------------

mat1<-matrix(c(1,2,3,4),nrow=2,byrow = T)
mat2<-matrix(c("a","b","c","d"),nrow=2,byrow = T)
mat3<-matrix(c(T,F,T,F),nrow=2,byrow = T)

mat1[1,]
mat1[,1]
mat1[2,1]

t(mat1)

mean(mat1[,2])
mean(mat1[2,])
#--------------------

a1 <- array(1:24,dim = c(2,4,3))
a1[1,2,3]
a1[2,,1]
#-----------------
  
my_data<-c("Male","Female","Female","Male")
my_data <- as.factor(my_data)

#----------------

df <- data.frame(Name=c("Sam","Bob"),Age=c(32,48),stringsAsFactors=F)

customer_churn <- read.csv("/Users/abhishekdas/Desktop/Intellipat/Data_Science_Course_Documents/customer_churn.csv",stringsAsFactors = F)

customer_churn$gender

#df <- data.frame(customer_churn[1:20,c(1,2,3,4)])

a <- customer_churn[,c(1,3,6)]
b <- customer_churn[,2:5]

customer_churn[3,]
customer_churn[c(3,5,7),]
customer_churn[5:10,]

customer_churn[4:8,2:5]
customer_churn[50:60,c(2,3)]
customer_churn[c(100:200,1000:2000),5:8]

#----------------------------------------
#Decision Making

    if(10>20){
    print("10 is less than 20")
  }

#------------
  if(10<20){
    print("10 is less than 20")
  }
#-----------
  if(10>20){
    print("10 is less than 20")
  }else{
    print("10 is greater than 20")
  }
#-----------------------------

customer_churn$TotalCharges <- ifelse(is.na(customer_churn$TotalCharges),0,customer_churn$TotalCharges)

#Looping

  a<-1:9

for (i in a) {
  print(i*2)
}
#------------
  i=1

while (i<=10) {
  print(i+2)
  i<-i+1
}
#--------
#inbuilt Functions

#colnames
#summary
  
str(customer_churn)

head(customer_churn)
head(customer_churn,10)
head(customer_churn,2)

tail(customer_churn)
tail(customer_churn,3)
tail(customer_churn,10)


nrow(customer_churn)
ncol(customer_churn)

dim(customer_churn)

max(c(1,2,3,4,5))
max(customer_churn$MonthlyCharges)

min(c(1,2,3,4,5))
min(customer_churn$MonthlyCharges)

mean(c(1,2,3,4,5))
mean(customer_churn$MonthlyCharges)

range(c(1,2,3,4,5))
range(customer_churn$MonthlyCharges)

sample(1:100,3)
sample(customer_churn$customerID,5)
sample(customer_churn$customerID,20)

table(customer_churn$gender)
table(customer_churn$InternetService)
#-------------------------------------------
data.frame(Name=c("Sam","Bob"),Marks=c(97,25)) -> student  
as.character(student$Name) -> student$Name
student <- rbind(student,c("Anne",75))
#---------------
data.frame(Name=c("Sam","Bob"),Marks=c(97,25)) -> student  
as.character(student$Name) -> student$Name
cbind(student,Grade=c("A","C"))
#-------------------
data.frame(Department=c("Tech","Analytics","Support"),Location=c("Chicago","New York","Boston")) -> Employee  

data.frame(Name=c("Sam","Bob","Anne"),Salary=c(75000,105000,120000),Department=c("Tech","Sales","Analytics")) -> Department

merge(Employee,Department,by="Department")
merge(Employee,Department,by="Department",all = T)
merge(Employee,Department,by="Department",all.x  = T)
merge(Employee,Department,by="Department",all.y  = T)
#---------------
Add_five<-function(x){
    x+5
  }

Add_five(3)
Add_five(c(10,15,20))

#############################
