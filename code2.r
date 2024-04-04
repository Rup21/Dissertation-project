#			========================= 
#		crime against women & children comparison 
# 			=========================

###			[call the data... remove na data... separate state and territory]

rm(list=ls())
crime.data=read.csv("C:/Users/majum/OneDrive - ST. XAVIER'S COLLEGE/#Dissertation paper/#work/CSVs/crime_against_data.csv")
crime.data=na.omit(crime.data)
attach(crime.data);crime.data
cds=crime.data.state= crime.data[type=="state",];crime.data.state
cdt=crime.data.territory= crime.data[type=="territory",];crime.data.territory
cds=cds[order(cds$population,decreasing=T),];cds
cdt=cdt[order(cdt$population,decreasing=T),];cdt
###			[make the matrix and make the bar plots for crime against women]

caw.state= matrix(c(cds$CAW2019,cds$CAW2020,cds$CAW2021),ncol=3,byrow=F);caw.state
caw.territory= matrix(c(cdt$CAW2019,cdt$CAW2020,cdt$CAW2021),ncol=3,byrow=F);caw.territory
colnames(caw.state)=colnames(caw.territory)=c("2019","2020","2021")

par(mfrow=c(3,1))
barplot(t(caw.state[1:18,]),beside=T,names.arg=cds$names[1:18],legend.text=T,
xlab="states of India",main="Crime against women for states",cex.names=0.6)
barplot(t(caw.state[19:28,]),beside=T,names.arg=cds$names[19:28],legend.text=T,
xlab="states of India",main="Crime against women for states",cex.names=0.6)
barplot(t(log(caw.territory)),beside=T,names.arg=cdt$names,legend.text=T,
xlab="territory of India",main="Crime against women for territory",cex.names=0.6)

###			[make the matrix and make the bar plots for crime against children]

cac.state= matrix(c(cds$CAC2019,cds$CAC2020,cds$CAC2021),ncol=3,byrow=F);cac.state
cac.territory= matrix(c(cdt$CAC2019,cdt$CAC2020,cdt$CAC2021),ncol=3,byrow=F);cac.territory
colnames(cac.state)=colnames(cac.territory)=c("2019","2020","2021")

par(mfrow=c(3,1))
barplot(t(cac.state[1:18,]),beside=T,names.arg=cds$names[1:18],legend.text=T,
xlab="states of India",main="Crime against children for states",cex.names=0.8)
barplot(t(cac.state[19:28,]),beside=T,names.arg=cds$names[19:28],legend.text=T,
xlab="states of India",main="Crime against children for states",cex.names=0.8)
barplot(t(log(cac.territory)),beside=T,names.arg=cdt$names,legend.text=T,
xlab="territory of India",main="Crime against children for territory",cex.names=0.8)

