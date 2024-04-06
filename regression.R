rm(list=ls())
reg.data=read.csv("C:/Users/majum/OneDrive - ST. XAVIER'S COLLEGE/#Dissertation paper/#work/CSVs/regression_data.csv")
reg.data=na.omit(reg.data)
attach(reg.data)
reg.data

y1=Rate_IPC;y2=Rate_SLL
X1=UER_rural;X2=UER_urban;X3=log(GDP);X4=log(NSDP);X5=CPI_gen;X6=abs(CPI_food)
data=data.frame(y1,y2,X1,X2,X3,X4,X5,X6,X7,X8);data

# raw data regression 
model1 = lm(y1~ X1+X2+X3+X4+X5+X6+X7+X8);model1;summary(model1)
model2 = lm(y2~ X1+X2+X3+X4+X5+X6+X7+X8);model2;summary(model2)


# final regression
data1=data[-31,-2];data2= data[,-1]
data1 
model_1= lm(y1~X3,data=data1);model_1;summary(model_1)
data2
model_2= lm(y2~ X3,data=data2);model_2;summary(model_2)