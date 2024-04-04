rm(list=ls())
reg.data=read.csv("C:/Users/majum/OneDrive - ST. XAVIER'S COLLEGE/#Dissertation paper/#work/CSVs/regression_data.csv")
reg.data=na.omit(reg.data)
attach(reg.data)
reg.data
#Y1= IPC/total_crime;Y2=SLL/total_crime
y1=Rate_IPC;y2=Rate_SLL
X1=UER_rural;X2=UER_urban;X3=log(GDP);X4=log(NSDP);X5=CPI_gen;X6=abs(CPI_food)
X7=as.vector(variable)
new_data=data.frame(y1,y2,X1,X2,X3,X4,X5,X6,X7);new_data

#	===================== 
#      Heteroscedasticity
#	=====================
x=X1;y=y2
m2=lm(y2~X1+X2+X3+X4+X5+X6+X7)
ui=abs(m2$resid);ui
x1=1/x					#case1
model1=summary(lm(ui~x1));model1
x2=1/sqrt(x)				#case2
model2=summary(lm(ui~x2));model2				#needs to recheck this portion
x3=sqrt(x)					#case3
model3=summary(lm(ui~x3));model3
x4=x						#case4
model4=summary(lm(ui~x4));model4

model.1=summary(lm(y[1:11]~x4[1:11]));model.1
rss1=186.1
model.2=summary(lm(y[23:33]~x4[23:33]));model.2
rss2=170
F=rss2/rss1;F
qf(.95,9,9)			# hypothesis rejected... homoscedastic
