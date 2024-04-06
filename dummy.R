rm(list=ls())
library(car)
reg.data=read.csv("C:/Users/majum/OneDrive - ST. XAVIER'S COLLEGE/#Dissertation paper/#work/CSVs/regression_data.csv")
reg.data=na.omit(reg.data)
attach(reg.data)
reg.data

y1=Rate_IPC;y2=Rate_SLL
X1=UER_rural;X2=UER_urban;X3=log(GDP);X4=log(NSDP);X5=CPI_gen;X6=abs(CPI_food)

model.1=lm(y1~X1+X2+X3+X4+X5+X6);summary(model.1)
vif(model.1)
model.2=lm(y2~X1+X2+X3+X4+X5+X6);summary(model.2)
vif(model.2)

dummy_popln1=ifelse(population>500,0,1);dummy_popln1		# popln < 500 assigned 1
dummy_popln2=ifelse(population>1000,0,ifelse(population>500,1,0));dummy_popln2  # 500<popln<1000 assigned 1

new_data=data.frame(y1,y2,X1,X2,X3,X4,X5,X6,dummy_popln1,dummy_popln2);new_data

model_1=lm(y1~X1+X2+X3+X4+X5+X6+X7+X8);summary(model_1)
vif(model_1)
model_2=lm(y2~X1+X2+X3+X4+X5+X6+X7+X8);summary(model_2)
vif(model_2)
