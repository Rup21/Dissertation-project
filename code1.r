rm(list=ls())
reg.data=read.csv("C:/Users/majum/OneDrive - ST. XAVIER'S COLLEGE/#Dissertation paper/#work/CSVs/regression_data.csv")
reg.data=na.omit(reg.data)
attach(reg.data)
reg.data

#	===================== 
#      Graphical Plotting
#	=====================

#Y1= IPC/total_crime;Y2=SLL/total_crime
y1=Rate_IPC;y2=Rate_SLL
X1=UER_rural;X2=UER_urban;X3=log(GDP);X4=log(NSDP);X5=CPI_gen;X6=abs(CPI_food)
X7=as.vector(variable);X8=as.vector(crim_cat)
new_data=data.frame(y1,y2,X1,X2,X3,X4,X5,X6,X7,X8);new_data

par(mfrow=c(1,2));plot(X1,y1);plot(X2,y1)
par(mfrow=c(1,2));plot(X3,y1);plot(X4,y1)
par(mfrow=c(1,2));plot(X5,y1);plot(X6,y1)


#	===================== 
#    Regression diagonostics
#	=====================

#M1=lm(Y1~X1+X2+X3+X4+X5+X6);M1;summary(M1)
#M2=lm(Y2~X1+X2+X3+X4+X5+X6);M2;summary(M2)

m1=lm(y1~X1+X2+X3+X4+X5+X6+X7,data=new_data);m1;summary(m1)		# check for model fit in case1
m2=lm(y2~X1+X2+X3+X4+X5+X6+X7+X8,data=new_data);m2;summary(m2)		# check for model fit in case2

##### in_between calculations are done in minitab #####

#correction of IPC data model (deleting influencial point)
new_data2= new_data[-c(31),-2];new_data2					# omit 31st obs.
new_m1=lm(y1~X1+X2+X3+X4+X5+X6+X7+X8,data=new_data2);new_m1
summary(new_m1)						# multiple r squared incresed from 39.22% to 52.87%

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

vif(lm(y2~X1+X2+X3+X4+X5+X6+X8))