#Assignement 5 - MLR - Computer Data

Comp_Data1<-read.csv('E:\\Sree-Official\\Sree-Personal\\EXCELR\\Data Science\\Assignments\\Multi Linear Regression 5\\Computer_Data.csv')
View(Comp_Data1)
Comp_data=Comp_Data1[,-c(1)]
attach(Comp_data)

#measures of central tendancy
mean(price)
median(price)
#mode
getmode <- function(x){
  uniquv <- unique(x)
  uniquv[which.max(tabulate(match(x,uniquv)))]
}
getmode(price)

#Measures of Dispersion
var(price)
sd(price)
range(price)
rangevalue <- function(x){max(x)-min(x)}
rangevalue(price)

# Measures of skweness & kurtosis
library(moments)
skewness(price)
kurtosis(price)

hist(price)
hist(speed)
hist(hd)
hist(ram)
hist(screen)

# Convert Factor Variables to Numeric
Comp_data$cd<-as.numeric(Comp_data$cd)
Comp_data$multi<-as.numeric(Comp_data$multi)
Comp_data$premium<-as.numeric(Comp_data$premium)
View(Comp_data)

pairs(Comp_data)
Correlation=cor(Comp_data)

#1-Regression Model and Summary
Computer_model=lm(price~.,data=Comp_data)
summary(Computer_model)

#Multi-colinearity
install.packages("car")
library(car)
car::vif(Computer_model)

plot(Computer_model)
residualPlots(Computer_model)
avPlots(Computer_model)
qqPlot(Computer_model)
influenceIndexPlot(Computer_model)

# Iteration 1
Comp_Data2=Comp_data[-c(1441,1701),]
Computer_model_2=lm(price~.,data=Comp_Data2)
summary(Computer_model_2)

plot(Computer_model_2)
residualPlots(Computer_model_2)
avPlots(Computer_model_2)
qqPlot(Computer_model_2)
influenceIndexPlot(Computer_model_2)

# Iteration 2 & Tranformation 1
Comp_Data3=Comp_data[-c(1441,1701,20,25),]
Comp_Data3['hd2']=Comp_Data3$hd*Comp_Data3$hd
Comp_Data3['speed2']=Comp_Data3$speed*Comp_Data3$speed
Computer_model_3=lm(price~.,data=Comp_Data3)
summary(Computer_model_3)

plot(Computer_model_3)
residualPlots(Computer_model_3)
avPlots(Computer_model_3)
qqPlot(Computer_model_3)
influenceIndexPlot(Computer_model_3)


library(MASS)
stepAIC(Computer_model)
stepAIC(Computer_model_2)
stepAIC(Computer_model_3)

##Predict for new data
attach(Comp_Data3)
colnames(Comp_Data3)

pred=predict(Computer_model_3)
pred

pred=predict(Computer_model_3)
Computer_Final<-data.frame(Comp_Data3,pred,"Error"=Comp_Data3$price-pred)

write.csv(Computer_Final, "Computer_Final.csv", row.names = FALSE)
