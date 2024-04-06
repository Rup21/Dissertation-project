rm(list=ls())
reg.data=read.csv("C:/Users/majum/OneDrive - ST. XAVIER'S COLLEGE/#Dissertation paper/#work/CSVs/regression_data.csv")
reg.data=na.omit(reg.data)
attach(reg.data)
reg.data

#	===================== 
#      Graphical Plotting(scatter plot)
#	=====================

y1=Rate_IPC;y2=Rate_SLL
X1=UER_rural;X2=UER_urban;X3=log(GDP);X4=log(NSDP);X5=CPI_gen;X6=abs(CPI_food)
new_data=data.frame(y1,y2,X1,X2,X3,X4,X5,X6,X7,X8);new_data

par(mfrow=c(1,2));plot(X1,y1);plot(X2,y1)
par(mfrow=c(1,2));plot(X3,y1);plot(X4,y1)		#case 1
par(mfrow=c(1,2));plot(X5,y1);plot(X6,y1)

par(mfrow=c(1,2));plot(X1,y2);plot(X2,y2)
par(mfrow=c(1,2));plot(X3,y2);plot(X4,y2)		#case 2
par(mfrow=c(1,2));plot(X5,y2);plot(X6,y2)
