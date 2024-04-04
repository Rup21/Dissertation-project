#		 ================== 
#		crime rate comparison 
#		 ==================

###			[call the data... remove na data... separate state and territory]

rm(list=ls())
crimerate.data=read.csv("C:/Users/majum/OneDrive - ST. XAVIER'S COLLEGE/#Dissertation paper/#work/CSVs/crimerate_comparison_data.csv")
crimerate.data=na.omit(crimerate.data)
attach(crimerate.data);crimerate.data
crds=crimerate.data.state= crimerate.data[type=="state",];crimerate.data.state
crdt=crimerate.data.territory= crimerate.data[type=="territory",];crimerate.data.territory

###			[make the matrix and make the bar plots]

crds.mat= matrix(c(crds$RCC_IPC,crds$RCC_SLL),ncol=2,byrow=F);crds.mat
crdt.mat= matrix(c(crdt$RCC_IPC,crdt$RCC_SLL),ncol=2,byrow=F);crdt.mat
colnames(ccrds.mat)=colnames(crdt.mat)=c("RCC_IPC","RCC_SLL")

par(mfrow=c(3,1))
barplot(t(crds.mat[1:14,]),beside=T,names.arg=crds$names[1:14],legend.text=T,xlab="states of India(1)",cex.names=0.6)
barplot(t(crds.mat[15:28,]),beside=T,names.arg=crds$names[15:28],legend.text=T,xlab="states of India(2)",cex.names=0.6)
barplot(t(crdt.mat),beside=T,names.arg=crdt$names,legend.text=T,xlab="territory of India",cex.names=0.6)