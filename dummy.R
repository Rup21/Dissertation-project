rm(list=ls())
library(car)
reg.data=read.csv("C:/Users/sony/OneDrive - ST. XAVIER'S COLLEGE/#Dissertation paper/#work/CSVs/regression_data.csv")
reg.data=na.omit(reg.data)
attach(reg.data)
reg.data

y1=Rate_IPC;y2=Rate_SLL
X1=UER_rural;X2=UER_urban;X3=log(GDP);X4=log(NSDP);X5=CPI_gen;X6=abs(CPI_food)

model.1=lm(y1~X1+X2+X3+X4+X5+X6);summary(model.1)
vif(model.1)
model.2=lm(y2~X1+X2+X3+X4+X5+X6);summary(model.2)
vif(model.2)

dummy_popln= ifelse(population>500,ifelse(population>1000,4,3),ifelse(population>100,2,1));dummy_popln
X7=as.vector(dummy_popln)
dummy_crime=  ifelse(total_crime>100000,ifelse(total_crime>500000,4,3),ifelse(total_crime>10000,2,1));dummy_crime
X8=as.vector(dummy_crime)
new_data=data.frame(y1,y2,X1,X2,X3,X4,X5,X6,X7,X8);new_data


model_1=lm(y1~X1+X2+X3+X4+X5+X6+X7+X8);summary(model_1)
vif(model_1)
model_2=lm(y2~X1+X2+X3+X4+X5+X6+X7+X8);summary(model_2)
vif(model_2)