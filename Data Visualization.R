customer_churn <- read.csv("/Users/abhishekdas/Desktop/Intellipat/Data_Science_Course_Documents/customer_churn.csv")

library(ggplot2)
library(dplyr)

# sapply(customer_churn, function(x) length(unique(x)))
# colSums(is.na(customer_churn))

plot(customer_churn$Dependents)
plot(customer_churn$Dependents, col="coral")
plot(customer_churn$Dependents, col="coral",xlab="Dependents")
#------
plot(customer_churn$PhoneService)  
plot(customer_churn$PhoneService,col="aquamarine4")
plot(customer_churn$PhoneService,col="aquamarine4",xlab="Phone Service",main="Distribution of PhoneService")
#--------
plot(customer_churn$Contract)  
plot(customer_churn$Contract,col="palegreen4",xlab="Contract",ylab = "Frequency",main="Distribution of Contract")


#---------------------------
hist(customer_churn$tenure)  
hist(customer_churn$tenure,col="olivedrab", xlab="Tenure",main="Distribution of Tenure")
hist(customer_churn$tenure,col="olivedrab", breaks=40)
#--------------------------------
plot(density(customer_churn$tenure))  
plot(density(customer_churn$tenure),col="blue",main="Density plot of Tenure")
#---------------------
ggplot(data=customer_churn,aes(x=tenure))+geom_histogram()  
ggplot(data = customer_churn, aes(x=tenure))+geom_histogram(bins = 50)
ggplot(data = customer_churn, aes(x=tenure))+geom_histogram(fill="palegreen4")
ggplot(data = customer_churn, aes(x=tenure, fill=Partner))+geom_histogram()+xlab("Tenure")+ylab("Frequency")
#ggplot(data = customer_churn, aes(x=tenure, fill=Partner))+geom_histogram(position="identity")
#-------------------------
ggplot(data = customer_churn,aes(x=Dependents))+geom_bar()
ggplot(data = customer_churn,aes(x=Dependents))+geom_bar(fill="chocolate")
ggplot(data = customer_churn,aes(x=Dependents,fill=DeviceProtection))+geom_bar()
ggplot(data = customer_churn,aes(x=Dependents,fill=DeviceProtection))+geom_bar(position="dodge",col="black")
#------------------
ggplot(data = customer_churn,aes(y=TotalCharges,x=tenure))+geom_point()  
ggplot(data = customer_churn,aes(y=TotalCharges,x=tenure))+geom_point(col="slateblue3")
ggplot(data = customer_churn,aes(y=TotalCharges,x=tenure, col=Partner))+geom_point() 
ggplot(data = customer_churn,aes(y=TotalCharges,x=tenure, col=InternetService))+geom_point() +
  scale_color_manual(values = c("steelblue2","red1","yellow2"))
ggplot(data = customer_churn,aes(y=TotalCharges,x=tenure, col=OnlineSecurity))+geom_point()
#-------
ggplot(data = customer_churn,aes(y=TotalCharges,x=MonthlyCharges, col=InternetService))+geom_point()

#------------------------
ggplot(data = customer_churn,aes(y=MonthlyCharges,x=Dependents))+geom_boxplot()  
ggplot(data = customer_churn,aes(y=MonthlyCharges,x=Dependents))+geom_boxplot(fill="yellowgreen")

ggplot(data = customer_churn,aes(y=MonthlyCharges,x=InternetService))+geom_boxplot()
ggplot(data = customer_churn,aes(y=MonthlyCharges,x=InternetService))+geom_boxplot(fill="violetred4")

ggplot(data = customer_churn,aes(y=MonthlyCharges,x=PaymentMethod))+geom_boxplot()

ggplot(data = customer_churn,aes(y=MonthlyCharges,x=PaymentMethod,fill=PaperlessBilling))+geom_boxplot()
#---------------------------
ggplot(data = customer_churn,aes(x=tenure,fill=InternetService))+geom_histogram(bins = 40) +
  facet_grid(~InternetService) + ggtitle("Tenure Histogram- Internet Service Wise") +
  xlab("Tenure") + ylab("Count") + theme_minimal()

ggplot(data = customer_churn,aes(y=TotalCharges,x=tenure, col=Contract)) + geom_point() + 
  facet_grid(~Contract) + xlab("Tenure") + ylab("Total Charges") +
  ggtitle("Total Charges vs. Tenure") 

#----------------------------
ggplot(data = customer_churn,aes(x=tenure))+geom_histogram(fill="tomato3", col="mediumaquamarine")->g1
g1+labs(title = "Distribution of tenure")->g2
g2+theme(panel.background = element_rect(fill = "olivedrab3"))->g3
g3+theme(plot.background = element_rect(fill = "palegreen4"))->g4
g4+theme(plot.title = element_text(hjust = 0.5, face="bold", colour = "peachpuff"))

#-------------------------

?airquality
airquality <- airquality
hist(airquality$Ozone)
plot(airquality$Ozone,airquality$Temp)
View(airquality)
plot(airquality)
  

ggplot(data=customer_churn,aes(x=tenure))+geom_histogram()  
ggplot(data = customer_churn, aes(x=tenure))+geom_histogram(bins = 20)
ggplot(data = customer_churn, aes(x=tenure))+geom_histogram(fill="palegreen4")
ggplot(data = customer_churn, aes(x=tenure, fill=Partner))+geom_histogram()+xlab("Tenure")+ylab("Frequency")
#ggplot(data = customer_churn, aes(x=tenure, fill=Partner))+geom_histogram(position="identity")
#-------------------------
ggplot(data = customer_churn,aes(x=Dependents))+geom_bar()
ggplot(data = customer_churn,aes(x=Dependents))+geom_bar(fill="chocolate")
ggplot(data = customer_churn,aes(x=Dependents,fill=DeviceProtection))+geom_bar()
ggplot(data = customer_churn,aes(x=Dependents,fill=DeviceProtection))+geom_bar(position="dodge",col="black")

ggplot(data = customer_churn,aes(x=Dependents,fill=DeviceProtection))+geom_bar(position="dodge",col="black")+labs(title = "Distribution of Dependents",x="Dependents",y="Frequency")

ggplot(data = customer_churn,aes(x=tenure,y=TotalCharges,col=DeviceProtection))+geom_bar(position="dodge",col="black")+labs(title = "Distribution of Dependents",x="Dependents",y="Frequency")

ggplot(data = customer_churn,aes(x=tenure,fill=InternetService))+geom_histogram(bins = 40) +
  facet_grid(~InternetService) + ggtitle("Tenure Histogram- Internet Service Wise") +
  xlab("Tenure") + ylab("Count") + theme_minimal()

ggplot(data = customer_churn,aes(y=TotalCharges,x=tenure, col=Contract)) + geom_point() + 
  facet_grid(~Contract) + xlab("Tenure") + ylab("Total Charges") +
  ggtitle("Total Charges vs. Tenure")


ggplot(data = customer_churn,aes(y=TotalCharges,x=tenure))+geom_point()  
ggplot(data = customer_churn,aes(y=TotalCharges,x=tenure))+geom_point(col="slateblue3")
ggplot(data = customer_churn,aes(y=TotalCharges,x=tenure, col=Partner))+geom_point() 
ggplot(data = customer_churn,aes(y=TotalCharges,x=tenure, col=InternetService))+geom_point() +
  scale_color_manual(values = c("steelblue2","red1","yellow2"))
ggplot(data = customer_churn,aes(y=TotalCharges,x=tenure, col=OnlineSecurity))+geom_point()
#-------
ggplot(data = customer_churn,aes(y=TotalCharges,x=MonthlyCharges, col=InternetService))+geom_point()

#------------------------
ggplot(data = customer_churn,aes(y=MonthlyCharges,x=Dependents))+geom_boxplot()  
ggplot(data = customer_churn,aes(y=MonthlyCharges,x=Dependents))+geom_boxplot(fill="yellowgreen")

ggplot(data = customer_churn,aes(y=MonthlyCharges,x=InternetService))+geom_boxplot()
ggplot(data = customer_churn,aes(y=MonthlyCharges,x=InternetService))+geom_boxplot(fill="violetred4")

ggplot(data = customer_churn,aes(y=MonthlyCharges,x=PaymentMethod))+geom_boxplot()

ggplot(data = customer_churn,aes(y=MonthlyCharges,x=PaymentMethod,fill=PaperlessBilling))+geom_boxplot()