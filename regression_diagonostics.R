rm(list=ls())
reg.data=read.csv("C:/Users/majum/OneDrive - ST. XAVIER'S COLLEGE/#Dissertation paper/#work/CSVs/regression_data.csv")
reg.data=na.omit(reg.data)
attach(reg.data)
reg.data

y1=Rate_IPC;y2=Rate_SLL
X1=UER_rural;X2=UER_urban;X3=log(GDP);X4=log(NSDP);X5=CPI_gen;X6=abs(CPI_food)
new_data=data.frame(y1,y2,X1,X2,X3,X4,X5,X6,X7,X8);new_data

#	===================== 
#      Influencial points
#	=====================

m1=lm(y1~X1+X2+X3+X4+X5+X6+X7+X8,data=new_data);m1;summary(m1)		# check for model fit in case1
m2=lm(y2~X1+X2+X3+X4+X5+X6+X7+X8,data=new_data);m2;summary(m2)		# check for model fit in case2

##### in_between calculations are done in minitab #####

#correction of IPC data model (deleting influencial point)
new_data2= new_data[-c(31),-2];new_data2					# omit 31st obs.
new_m1=lm(y1~X1+X2+X3+X4+X5+X6+X7+X8,data=new_data2);new_m1;summary(new_m1)

#	===================== 
#	  Multicollinearity 
#	=====================
library(ggplot2)
library(reshape2) # for melt function
library(car)	# for vif

cor_matrix1=cor(new_data2);cor_matrix1	# Calculate correlation matrix
cor_df1=melt(cor_matrix1)				# Convert correlation matrix to long format
heatmap_plot1=ggplot(cor_df1,aes(Var1,Var2,fill=value,label=round(value,4)))+
		   geom_tile()+geom_text(color="black",size=3)+				 # Add correlation values
 		   scale_fill_gradient2(low="blue",mid="white",high="red",
		     midpoint=0,limits=c(-1, 1))+						 # color & scale
		   labs(title="Correlation Heatmap(for case1)",x ="",y ="")+ 	 # Labels
  		   theme_minimal()+								 # Theme options
  		   theme(axis.text.x=element_text(angle=0,hjust=1)) 	 # Rotate x-axis labels
print(heatmap_plot1)

cor_matrix2=cor(new_data[,-1]);cor_matrix2	# Calculate correlation matrix
cor_df2=melt(cor_matrix2)				# Convert correlation matrix to long format
heatmap_plot2=ggplot(cor_df2,aes(Var1,Var2,fill=value,label=round(value,4)))+
		   geom_tile()+geom_text(color="black",size=3)+				 # Add correlation values
 		   scale_fill_gradient2(low="blue",mid="white",high="red",
		     midpoint=0,limits=c(-1, 1))+						 # color & scale
		   labs(title="Correlation Heatmap(for case2)",x ="",y ="")+ 	 # Labels
  		   theme_minimal()+								 # Theme options
  		   theme(axis.text.x=element_text(angle=0,hjust=1)) 	 # Rotate x-axis labels
print(heatmap_plot2)

model_1=new_m1;model_2=m2
vif(model_1)				# collinearity for case1
vif(model_2)				# collinearity for case2

#	===================== 
#      Heteroscedasticity
#	=====================
plot(fitted(model_1),model_1$resid,ylab="residual value of the model",xlab="fitted value of y1")
plot(fitted(model_2),model_2$resid,ylab="residual value of the model",xlab="fitted value of y2")
# no pattern found.. we till proceed for testing to reconfirm it

# =========== case 1 ================
m2=lm(y1~X1+X2+X3+X4+X5+X6+X7+X8)
ui=abs(m2$resid);ui

# variable x1
x=X1
x1=1/x					# delta2= -1
model1=summary(lm(ui~x1));model1
x2=1/sqrt(x)				# delta2= -1/2
model2=summary(lm(ui~x2));model2				
x3=sqrt(x)					# delta2= 1/2
model3=summary(lm(ui~x3));model3
x4=x						# delta2= 1
model4=summary(lm(ui~x4));model4

##none of the value is significant

# variable x2
x=X2
x1=1/x					# delta2= -1
model1=summary(lm(ui~x1));model1
x2=1/sqrt(x)				# delta2= -1/2
model2=summary(lm(ui~x2));model2				
x3=sqrt(x)					# delta2= 1/2
model3=summary(lm(ui~x3));model3
x4=x						# delta2= 1
model4=summary(lm(ui~x4));model4

model.1=summary(lm(y1[1:11]~x4[1:11]));model.1		# Goldfeldt-Quant test
rss1=181
model.2=summary(lm(y1[23:33]~x4[23:33]));model.2
rss2=382.2
F=rss2/rss1;F;qf(.95,9,9)	# hypothesis rejected

# variable x3
x=X3
x1=1/x					# delta2= -1
model1=summary(lm(ui~x1));model1
x2=1/sqrt(x)				# delta2= -1/2
model2=summary(lm(ui~x2));model2				
x3=sqrt(x)					# delta2= 1/2
model3=summary(lm(ui~x3));model3
x4=x						# delta2= 1
model4=summary(lm(ui~x4));model4

##none of the value is significant

# variable x4
x=X4
x1=1/x					# delta2= -1
model1=summary(lm(ui~x1));model1
x2=1/sqrt(x)				# delta2= -1/2
model2=summary(lm(ui~x2));model2				
x3=sqrt(x)					# delta2= 1/2
model3=summary(lm(ui~x3));model3
x4=x						# delta2= 1
model4=summary(lm(ui~x4));model4

##none of the value is significant

# variable x5
x=X5
x1=1/x					# delta2= -1
model1=summary(lm(ui~x1));model1
x2=1/sqrt(x)				# delta2= -1/2
model2=summary(lm(ui~x2));model2				
x3=sqrt(x)					# delta2= 1/2
model3=summary(lm(ui~x3));model3
x4=x						# delta2= 1
model4=summary(lm(ui~x4));model4

##none of the value is significant

# variable x6
x=X6
x1=1/x					# delta2= -1
model1=summary(lm(ui~x1));model1
x2=1/sqrt(x)				# delta2= -1/2
model2=summary(lm(ui~x2));model2				
x3=sqrt(x)					# delta2= 1/2
model3=summary(lm(ui~x3));model3
x4=x						# delta2= 1
model4=summary(lm(ui~x4));model4

model.1=summary(lm(y1[1:11]~x4[1:11]));model.1		# Goldfeldt-Quant test
rss1=103.3
model.2=summary(lm(y1[23:33]~x4[23:33]));model.2
rss2=304.5
F=rss2/rss1;F;qf(.95,9,9)		# hypothesis rejected

# variable X7 and X8 are binary... so not checked for Glejser test

# =========== case 2 ================
m2=lm(y2~X1+X2+X3+X4+X5+X6+X7+X8)
ui=abs(m2$resid);ui

# variable x1
x=X1
x1=1/x					# delta2= -1
model1=summary(lm(ui~x1));model1
x2=1/sqrt(x)				# delta2= -1/2
model2=summary(lm(ui~x2));model2				
x3=sqrt(x)					# delta2= 1/2
model3=summary(lm(ui~x3));model3
x4=x						# delta2= 1
model4=summary(lm(ui~x4));model4

##none of the value is significant

# variable x2
x=X2
x1=1/x					# delta2= -1
model1=summary(lm(ui~x1));model1
x2=1/sqrt(x)				# delta2= -1/2
model2=summary(lm(ui~x2));model2				
x3=sqrt(x)					# delta2= 1/2
model3=summary(lm(ui~x3));model3
x4=x						# delta2= 1
model4=summary(lm(ui~x4));model4

##none of the value is significant

# variable x3
x=X3
x1=1/x					# delta2= -1
model1=summary(lm(ui~x1));model1
x2=1/sqrt(x)				# delta2= -1/2
model2=summary(lm(ui~x2));model2				
x3=sqrt(x)					# delta2= 1/2
model3=summary(lm(ui~x3));model3
x4=x						# delta2= 1
model4=summary(lm(ui~x4));model4

##none of the value is significant

# variable x4
x=X4
x1=1/x					# delta2= -1
model1=summary(lm(ui~x1));model1
x2=1/sqrt(x)				# delta2= -1/2
model2=summary(lm(ui~x2));model2				
x3=sqrt(x)					# delta2= 1/2
model3=summary(lm(ui~x3));model3
x4=x						# delta2= 1
model4=summary(lm(ui~x4));model4

##none of the value is significant

# variable x2
x=X2
x1=1/x					# delta2= -1
model1=summary(lm(ui~x1));model1
x2=1/sqrt(x)				# delta2= -1/2
model2=summary(lm(ui~x2));model2				
x3=sqrt(x)					# delta2= 1/2
model3=summary(lm(ui~x3));model3
x4=x						# delta2= 1
model4=summary(lm(ui~x4));model4

##none of the value is significant

# variable x3
x=X3
x1=1/x					# delta2= -1
model1=summary(lm(ui~x1));model1
x2=1/sqrt(x)				# delta2= -1/2
model2=summary(lm(ui~x2));model2				
x3=sqrt(x)					# delta2= 1/2
model3=summary(lm(ui~x3));model3
x4=x						# delta2= 1
model4=summary(lm(ui~x4));model4

##none of the value is significant

# variable x4
x=X4
x1=1/x					# delta2= -1
model1=summary(lm(ui~x1));model1
x2=1/sqrt(x)				# delta2= -1/2
model2=summary(lm(ui~x2));model2				
x3=sqrt(x)					# delta2= 1/2
model3=summary(lm(ui~x3));model3
x4=x						# delta2= 1
model4=summary(lm(ui~x4));model4

##none of the value is significant

# variable x5
x=X5
x1=1/x					# delta2= -1
model1=summary(lm(ui~x1));model1
x2=1/sqrt(x)				# delta2= -1/2
model2=summary(lm(ui~x2));model2				
x3=sqrt(x)					# delta2= 1/2
model3=summary(lm(ui~x3));model3
x4=x						# delta2= 1
model4=summary(lm(ui~x4));model4

##none of the value is significant

# variable x6
x=X6
x1=1/x					# delta2= -1
model1=summary(lm(ui~x1));model1
x2=1/sqrt(x)				# delta2= -1/2
model2=summary(lm(ui~x2));model2				
x3=sqrt(x)					# delta2= 1/2
model3=summary(lm(ui~x3));model3
x4=x						# delta2= 1
model4=summary(lm(ui~x4));model4

##none of the value is significant
