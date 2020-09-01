customer_churn <- read.csv("/Users/abhishekdas/Desktop/Intellipat/Data_Science_Course_Documents/customer_churn.csv")

library(dplyr)

mat1<-matrix(c(1,2,3,4),nrow=2)
apply(mat1,1,sum)
apply(mat1,2,sum)

m <- matrix(c(1:10,11:20,31:40),nrow = 10, ncol = 3)
apply(m, 2 , function(x) length(x) - 1)

apply(customer_churn, 2 , function(x) sum(is.na(x)))
apply(customer_churn, 2 , function(x) length(unique(x)))
##----
my_list <- list(a=c(1,1),b=c(2,2),c=c(3,3))  
lapply(my_list, mean)
sapply(my_list, mean)

tapply(customer_churn$tenure, customer_churn$Partner, mean)
tapply(customer_churn$MonthlyCharges, customer_churn$Churn, mean)

#----------
x<-c(1:5) 
b<-c(6:10)
mapply(sum, x, b)

##----------
select(customer_churn,2) ->c_2
select(customer_churn,6) ->c_6
select(customer_churn,1,4,7,12) ->c_bunch
select(customer_churn,5:10) ->c_5_10
select(customer_churn,gender) ->c_gender
select(customer_churn,gender,Partner,tenure) ->c_gpt
select(customer_churn,gender:Contract) ->c_gender_contract
select(customer_churn,starts_with("Stream")) -> c_stream
select(customer_churn,ends_with("Charges")) -> c_charges
##----
filter(customer_churn,gender=="Female") -> c_female  
filter(customer_churn, MonthlyCharges>100)->c_high_paying_customers
filter(customer_churn,gender=="Female" & MonthlyCharges>100)->c_high_female
filter(customer_churn,StreamingTV=="Yes" & StreamingMovies=="Yes")->c_stream
filter(customer_churn,tenure>50 & InternetService == "DSL" & Contract=="One year")->c_tic
filter(customer_churn,PaymentMethod=="Electronic check" | PaymentMethod=="Mailed check" )->c_pay
filter(customer_churn,(Contract=="One year" | Contract=="Two years") & gender=="Female"  )->c_con_gen
filter(customer_churn,(InternetService == "DSL" | InternetService == "Fiber optic") &(tenure>50 & MonthlyCharges >100))->c_complicated
##------
mutate(customer_churn, Age= ifelse(SeniorCitizen==0, sample(x=16:55),sample(x=56:100))) -> customer_churn
mutate(customer_churn,customer_category=ifelse(MonthlyCharges < 45, "Low_Paying", 
                                          ifelse(MonthlyCharges< 90,"Medium_paying","High_paying")))-> customer_churn

customer_churn$churn_new <- ifelse(customer_churn$Churn=="Yes",1,0)

##---------
sample_n(customer_churn,10)->random_10
sample_n(customer_churn,100)->random_100
sample_frac(customer_churn,0.1)->random_10percent
sample_frac(customer_churn,0.5)->random_50percent
#--
summarise(customer_churn,mean_tenure=mean(tenure))  
summarise(customer_churn,mean_MC=mean(MonthlyCharges))
summarise(customer_churn,mean_TC=mean(TotalCharges,na.rm=T))
#------------
df1 <- summarise(group_by(customer_churn,InternetService),mean_tenure=mean(tenure))  
df2 <- summarise(group_by(customer_churn,Partner),mean_MonthlyCharges=mean(MonthlyCharges))
df3 <- summarise(group_by(customer_churn,InternetService),mean_MonthlyCharges=mean(MonthlyCharges))
###

new <- arrange(customer_churn, MonthlyCharges)
new1 <- arrange(customer_churn, desc(MonthlyCharges))

#---------
customer_churn %>% select(1:5) -> c_15  
customer_churn %>% select(1:5) %>% filter( gender =="Male") -> c_15_male
customer_churn %>% filter(InternetService=="DSL") %>% group_by(gender) %>% summarise(mean_mc=mean(MonthlyCharges)) -> df7
customer_churn %>% group_by(PaymentMethod) %>% summarise(mean_tenure=mean(tenure),count=n()) -> new_df
customer_churn %>% group_by(PaymentMethod) %>% summarise(mean_tenure=mean(tenure)) %>% arrange(desc(PaymentMethod)) -> df8
customer_churn %>% select(1,2,10:21) %>% filter(Contract=="One year" | Contract=="Two year") %>% arrange(Contract) -> c_contract
customer_churn %>% filter(PaperlessBilling=="No") %>% group_by(TechSupport) %>%summarise(mean_tenure=mean(tenure)) -> df9

################

x <- c(1,2,2,4,5,6,6,6,7,8,1,2,3,3,3)
duplicated(x)
x[duplicated(x)]
y <- x[!duplicated(x)]

##################

gmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v,uniqv)))]
}

v <- c(1,1,1,1,1,4,5,6,2,2,3,4,1,1,1,1,1)
gmode(v)

################

