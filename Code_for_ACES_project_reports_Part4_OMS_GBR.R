### Updated on Nov 14
### This code runs all data sets when we study the temporal counts 
### i.e. chronological evaluation of the
### Olympic Medal Scores for GBR Athletes
### parametric modeling for the total complete data, i.e. material 
### for Part 4 of the reports.


ticALL<-date()
library(moments)
library(poweRlaw)
library(car)
library(MASS) 
library(STAND)
library(nortest)
library(truncdist)
library(EnvStats)
library(igraph)

### Folder where we have the data
setwd("D:/Vitalii/Data")

### Here we will load the Olympic Medals Scores for USA athletes in chronological order
#tmpOMS_USA<-read.csv("OlympicMedalsScores_USA_year_cumulative_full.csv",header=T,sep=",")
#tmpOMS_USA<-read.csv("OlympicMedalsScores_USA_year_cumulative_full_without_Phelps.csv",header=T,sep=",")
#tmpOMS_USA<-read.csv("OlympicMedalsScores_USA_year_cumulative_trunc.csv",header=T,sep=",")
#tmpOMS_USA<-read.csv("OlympicMedalsScores_USA_year_cumulative_trunc_without_Phelps.csv",header=T,sep=",")
#tmpOMS_USA<-read.csv("OlympicMedalsScores_FRA_year_cumulative_full.csv",header=T,sep=",")
#tmpOMS_USA<-read.csv("OlympicMedalsScores_FRA_year_cumulative_trunc.csv",header=T,sep=",")

tmpOMS_USA<-read.csv("./Athletes/OlympicMedalsScores_GBR_year_cumulative_full.csv",header=T,sep=",")

#tmpOMS_USA<-read.csv("OlympicMedalsScores_GBR_year_cumulative_trunc.csv",header=T,sep=",")



OMS_USA_cumm_time<-tmpOMS_USA[,4]
CTV<-unique(OMS_USA_cumm_time)		### Cummulative Time Values

### Times at which we will study the evolution:
OMS_time<-c(6,11,15,19,26,35)		### 6=1912, 11=1936, 15=1960, 19=1976, 26=1998, 35=2000

OMS_USA_1912<-tmpOMS_USA[tmpOMS_USA[,4]==CTV[OMS_time[1]],2]		### USA olympic scores in 1912
OMS_USA_1936<-tmpOMS_USA[tmpOMS_USA[,4]==CTV[OMS_time[2]],2]		### USA olympic scores in 1936
OMS_USA_1960<-tmpOMS_USA[tmpOMS_USA[,4]==CTV[OMS_time[3]],2]		### USA olympic scores in 1960
OMS_USA_1976<-tmpOMS_USA[tmpOMS_USA[,4]==CTV[OMS_time[4]],2]		### USA olympic scores in 1976
OMS_USA_1998<-tmpOMS_USA[tmpOMS_USA[,4]==CTV[OMS_time[5]],2]		### USA olympic scores in 1998
OMS_USA_2016<-tmpOMS_USA[tmpOMS_USA[,4]==CTV[OMS_time[6]],2]		### USA olympic scores in 2016

Data<-list(OMS_USA_1912, OMS_USA_1936, OMS_USA_1960, OMS_USA_1976, OMS_USA_1998, OMS_USA_2016)
#VarNames<-c("OMS_USA_1912", "OMS_USA_1936", "OMS_USA_1960", "OMS_USA_1976", "OMS_USA_1998", "OMS_USA_2016")
#VarNames<-c("OMS_USA_1912", "OMS_USA_1936", "OMS_USA_1960", "OMS_USA_1976", "OMS_USA_1998", "OMS_USA_2016_no_Phelps")
#VarNames<-c("TR_OMS_USA_1912", "TR_OMS_USA_1936", "TR_OMS_USA_1960", "TR_OMS_USA_1976", "TR_OMS_USA_1998", "TR_OMS_USA_2016")
#VarNames<-c("TR_OMS_USA_1912", "TR_OMS_USA_1936", "TR_OMS_USA_1960", "TR_OMS_USA_1976", "TR_OMS_USA_1998", "TR_OMS_USA_2016_no_Phelps")
#VarNames<-c("OMS_FRA_1912", "OMS_FRA_1936", "OMS_FRA_1960", "OMS_FRA_1976", "OMS_FRA_1998", "OMS_FRA_2016")
#VarNames<-c("TR_OMS_FRA_1912", "TR_OMS_FRA_1936", "TR_OMS_FRA_1960", "TR_OMS_FRA_1976", "TR_OMS_FRA_1998", "TR_OMS_FRA_2016")

VarNames<-c("OMS_GBR_1912", "OMS_GBR_1936", "OMS_GBR_1960", "OMS_GBR_1976", "OMS_GBR_1998", "OMS_GBR_2016")

#VarNames<-c("TR_OMS_GBR_1912", "TR_OMS_GBR_1936", "TR_OMS_GBR_1960", "TR_OMS_GBR_1976", "TR_OMS_GBR_1998", "TR_OMS_GBR_2016")


n<-length(VarNames)			### This is the number of the recorded variables (i.e. columns in the list Data)


#pdf(file="C:\\Users\\PT_office\\Desktop\\Aces_project\\Plots_evolution\\OMS_USA_Full_chronological_evolution.pdf")
#pdf(file="C:\\Users\\PT_office\\Desktop\\Aces_project\\Plots_evolution\\OMS_USA_Full_chronological_evolution_without_Phelps.pdf")
#pdf(file="C:\\Users\\PT_office\\Desktop\\Aces_project\\Plots_evolution\\OMS_USA_Truncated_chronological_evolution.pdf")
#pdf(file="C:\\Users\\PT_office\\Desktop\\Aces_project\\Plots_evolution\\OMS_USA_Truncated_chronological_evolution_without_Phelps.pdf.pdf")
#pdf(file="C:\\Users\\PT_office\\Desktop\\Aces_project\\Plots_evolution\\OMS_FRA_Full_chronological_evolution.pdf")
#pdf(file="C:\\Users\\PT_office\\Desktop\\Aces_project\\Plots_evolution\\OMS_FRA_Truncated_chronological_evolution.pdf")
#pdf(file="C:\\Users\\PT_office\\Desktop\\Aces_project\\Plots_evolution\\OMS_GBR_Full_chronological_evolution.pdf")
#pdf(file="C:\\Users\\PT_office\\Desktop\\Aces_project\\Plots_evolution\\OMS_GBR_Truncated_chronological_evolution.pdf")



##################################
### Descriptive Statistics for each variable
##################################

### Here we will put the histograms along with the overlaid density plots
x11()
par(mfrow=c(2,3), mar=c(1,1,1,1),oma=c(1,1,1,0))
par(mgp=c(0,1,0))
for (i in 1:n){
	tmpData<-Data[[i]]
	hist(tmpData,freq=F,xlab="",ylab="",main="")
	#lines(density(tmpData),col="blue",lwd=1)
	legend("top",legend=paste(VarNames[i]), cex=2, bty="n")
}
title("Histograms of the OMS USA data sets",outer=T,cex.main=2,col.main="blue",line=-0.5)


SSall<-matrix(NA,n, 9)		### Summary stats: where rows are data sets and cols are (sample size, Min, Q1, Median, Q3, Max, Mean, SD, Skewness)
for (i in 1:n){
	Y<-Data[[i]]
	SSall[i,1]<-length(Y)			## sample size
	SSall[i,2:6]<-quantile(Y,c(0,0.25,0.5,0.75,1))		### Min, Q1, Median, Q3, Max
	SSall[i,7]<-mean(Y)				### mean of response variable
	SSall[i,8]<-	sd(Y)				### sd of response variable
	SSall[i,9]<-	skewness(Y)	### skewness of response variable
}

round(SSall,3)




############################################################
### HERE WE WILL RUN THE PARAMETRIC APPROACH TO EXAMINE 
### POWER-LAW AND LOGNORMAL DISTRIBUTIONS
############################################################


PLest<-matrix(NA,n,7)			### matrix with the estimated parameter values for the Discrete Power Law distribution
											### rows refer to different data sets and col1=n-tail, col2=xmin, col3=sd(xmin), col4=alpha, col5=sd(alpha)
											### col6=goodness of fit (gof) Kolmogorov-Smirnov, col7=Bootstrap p-value for PL fitting
LNest<-matrix(NA,n,9)			### matrix with the estimated parameter values for the Discrete LogNormal distribution
											### rows refer to different data sets and col1=n-tail, col2=xmin, col3=sd(xmin), col4=mu, col5=sd(mu)
											### col6=sigma, col7=sd(sigma), col8=goodness of fit (gof) Kolmogorov-Smirnov, col9=Bootstrap p-value for LN fitting
#BSiter<-10000			### Bootstrat iterations (the larger the number the longer it takes)
BSiter<-1000					### Bootstrat iterations for the paper we ran 10,000 iterations but it takes ~34 hours to run them, so we put 10 for illustration

bsPLall<-array(NA,dim=c(BSiter,4,n))		### In this array we will save the results of all the Bootstrap iterations in Power Law
																### where rows will refer to the number of bootstrap samples (BSiter), 														
																### col1=gof, col2=xmin, col3=alpha, col4=ntail and layers refer to the different data sets
bsPL_pall<-array(NA,dim=c(BSiter,4,n))		### In this array we will save the results of all the Bootstrap p-value iterations in Power Law
																	### where rows will refer to the number of bootstrap samples (BSiter), 														
																	### col1=gof, col2=xmin, col3=alpha, col4=ntail and layers refer to the different data sets
bsLNall<-array(NA,dim=c(BSiter,5,n))		### In this array we will save the results of all the Bootstrap iterations in Power Law
																### where rows will refer to the number of bootstrap samples (BSiter), 														
																### col1=gof, col2=xmin, col3=mean, col4=sigma, col5=ntail and layers refer to the different data sets
bsLN_pall<-array(NA,dim=c(BSiter,5,n))		### In this array we will save the results of all the Bootstrap p-value iterations in Power Law
																	### where rows will refer to the number of bootstrap samples (BSiter), 														
																	### col1=gof, col2=xmin, col3=mean, col4=sigma, col5=ntail and layers refer to the different data sets
tic<-date()
for (i in 2:n){
	x<-Data[[i]]
	### Here we fit the Power Law
	m_pl <- displ$new(x)
	estpl <- estimate_xmin(m_pl)
	m_pl$setXmin(estpl)
	PLest[i,1]<-estpl$ntail
	PLest[i,2]<-estpl$xmin
	PLest[i,4]<-estpl$pars
	PLest[i,6]<-estpl$gof
	bsPL <- bootstrap(m_pl, no_of_sims= BSiter, seed=1)		### here we will get the sd values
	bsPLall[,,i]<-matrix(unlist(bsPL[2]),ncol=4,byrow=F)
	#plot(bsPL,trim=0.1)
	PLest[i,3]<-sd(bsPL$bootstraps[, 2])		### sd for Xmin
	PLest[i,5]<-sd(bsPL$bootstraps[, 3])		### sd for alpha
	bsPL_p <- bootstrap_p(m_pl,no_of_sims= BSiter, seed=1)				### here we will get the p-value regarding the PL fit
	bsPL_pall[,,i]<-matrix(unlist(bsPL_p[3]),ncol=4,byrow=F)
	PLest[i,7]<-bsPL_p$p
	x11()
	plot(bsPL_p)
	legend("bottomright",legend=paste("Power Law \n",VarNames[i]), cex=1, bty="n")

	### Here we fit the Lognormal
	m_ln <- dislnorm$new(x)
	estln <- estimate_xmin(m_ln)
	m_ln$setXmin(estln)
	LNest[i,1]<-estln$ntail
	LNest[i,2]<-estln$xmin
	LNest[i,c(4,6)]<-estln$pars
	LNest[i,8]<-estln$gof
	bsLN <- bootstrap(m_ln, no_of_sims= BSiter, seed=1)		### here we will get the sd values
	bsLNall[,,i]<-matrix(unlist(bsLN[2]),ncol=5,byrow=F)
	#plot(bsLN,trim=0.1)
	LNest[i,3]<-sd(bsLN$bootstraps[, 2])		### sd for Xmin
	LNest[i,5]<-sd(bsLN$bootstraps[, 3])		### sd for mu
	LNest[i,7]<-sd(bsLN$bootstraps[, 4])		### sd for sigma
	bsLN_p <- bootstrap_p(m_ln,no_of_sims= BSiter, seed=1)		### here we will get the p-value regarding the LN fit
	bsLN_pall[,,i]<-matrix(unlist(bsLN_p[3]),ncol=5,byrow=F)
	LNest[i,9]<-bsLN_p$p
	x11()
	plot(bsLN_p)
	legend("bottomright",legend=paste("Lognormal \n",VarNames[i]), cex=1, bty="n")
	
	### Here we plot the two fits along with the bootstrap distribution of the parameters 			
	x11()
	par(mfrow=c(2,3), mar=c(1,2,3,1),oma=c(1,1,1,0))
	par(mgp=c(0,1,0))
	plot(m_pl,xlab="",ylab="")
	lines(m_pl, col="coral",lwd=2)
	lines(m_ln, col="blue",lwd=2)
	legend("topright",title="Power Law",title.col="coral",c(paste("n-tail =", round(PLest[i,1],3)),paste("GoF =", round(PLest[i,6],3)),paste("p-value =", round(PLest[i,7],3))), bty="n")
	legend("bottomleft",title="Lognormal",title.col="blue",c(paste("n-tail =", round(LNest[i,1],3)),paste("GoF =", round(LNest[i,8],3)),paste("p-value =", round(LNest[i,9],3))), bty="n")
	#legend("bottomleft",legend=c(paste("Power Law, p-val =", round(PLest[i,7],3)),paste("Lognormal, p-val =", round(LNest[i,9],3))), lty=c(1,1), lwd=c(2,2), col=c("coral","blue"), bty="n")
	hist(bsPL$bootstraps[,3],xlab="",ylab="",main=expression(paste("Power Law: ", alpha)),col="coral",col.main="coral")
	abline(v=PLest[i,4],lwd=3,lty=2)
	legend("topright",eval(substitute(expression(paste(hat(alpha)[PL],"=",v)),list(v=round(PLest[i,4],3)))),lty=2,lwd=3,bty="n") 
	#barplot(bsPL$bootstraps[,2],xlab="",ylab="",main=expression(paste("Power Law: ", x[min])),col="coral",col.main="coral")
	hist(bsPL$bootstraps[,2],breaks="fd",xlab="",ylab="",main=expression(paste("Power Law: ", x[min])),col="coral",col.main="coral")
	abline(v=PLest[i,2],lwd=3,lty=2)
	legend("topright",eval(substitute(expression(paste(hat(X)[min],"=",v)),list(v=round(PLest[i,2],3)))),lty=2,lwd=3,bty="n"  ) 
	hist(bsLN$bootstraps[,3],breaks="fd",xlab="",ylab="",main=expression(paste("Lognormal: ", mu)),col="blue",col.main="blue")
	abline(v=LNest[i,4],lwd=3,lty=2)
	legend("topright",eval(substitute(expression(paste(hat(mu)[LN],"=",v)),list(v=round(LNest[i,4],3)))),lty=2,lwd=3,bty="n") 
	hist(bsLN$bootstraps[,4],breaks="fd",xlab="",ylab="",main=expression(paste("Lognormal: ", sigma)),col="blue",col.main="blue")
	abline(v= LNest[i,6],lwd=3,lty=2)
	legend("topright",eval(substitute(expression(paste(hat(sigma)[LN],"=",v)),list(v=round(LNest[i,6],3)))),lty=2,lwd=3,bty="n") 
	#barplot(table(bsLN$bootstraps[,2]),xlab="",ylab="",main=expression(paste("Lognormal: ", x[min])),col="blue",col.main="blue")
	hist(bsLN$bootstraps[,2],breaks="fd",xlab="",ylab="",main=expression(paste("Lognormal: ", x[min])),col="blue",col.main="blue")
	abline(v= LNest[i,2],lwd=3,lty=2)
	legend("topright",eval(substitute(expression(paste(hat(X)[min],"=",v)),list(v=round(LNest[i,2],3)))),lty=2,lwd=3,bty="n"  ) 
	title(paste(VarNames[i], ": Power Law and Lognormal parameters"),outer=T,cex.main=2,col.main="black",line=-0.5)

print("************************************************************************************")
print(paste(VarNames[i], "is completed!!!", i/n, "data analysis completed"))
print("************************************************************************************")
tictoc<-date()
print(tictoc)
}
toc<-date()
tic
toc






###################################################################################################
### Here we will create the master-plot which will have the fit of the lognormal distribution only and will be used in the paper
###################################################################################################

x11()
par(mfrow=c(2,3), mar=c(2,3,1,1),oma=c(1,1,1,0))
par(mgp=c(0,1,0))
for (i in 1:n){
	x<-Data[[i]]
		### Here we fit the Lognormal
	m_ln <- dislnorm$new(x)
	estln <- estimate_xmin(m_ln)
	m_ln$setXmin(estln)
	plot(m_ln,xlab="",ylab="")
	lines(m_ln, col="red",lwd=2)
#	legend("bottomleft",title=paste(VarNames[i], "\n Lognormal"),title.col="blue",c(paste("n-tail =", round(LNest[i,1],3)),paste("GoF =", round(LNest[i,8],3)),paste("p-value =", round(LNest[i,9],3))), bty="n")
	rp = vector('expression',3)
	rp[1] = substitute(expression(italic(n)~-tail == MYVALUE), 
		list(MYVALUE = format(LNest[i,1], 3),dig=3))[2]
	rp[2] = paste("[",round(LNest[i,1]/length(Data[[i]])*100,1),"% ]")
	rp[3] = substitute(expression(italic(p)~-value == MYOTHERVALUE), 
		list(MYOTHERVALUE = format(LNest[i,9], digits = 3)))[2]
	legend("bottomleft",title=paste(VarNames[i], "\n Lognormal"),title.col="red", legend = rp, bty = 'n')  
}








#################################
### Here we will provide the overall plots 
#################################

###--------------------------------------------------------------------------------------------------------------------------------------------------------

### Barplots of the n-tail
###_________________
LData<-rep(NA, n)		### In this vector we will put the length of each of the data sets used in the analysis
MinData<-rep(NA, n)	### In this vector we will put the minimum integer value of each of the data sets
for (i in 1:n){
	LData[i]<-length(Data[[i]])
	MinData[i]<-min(Data[[i]])
}
LData
MinData


### This is the Table with Length of data in the col1 and then xmin, n-tail and % of data 
### used in the analysis to fit the Power Law (col2-4) and Lognormal (col5-7)
cbind(LData, MinData, PLest[,c(2,3,1)],PLest[,1]/LData,LNest[,c(2,3,1)],LNest[,1]/LData)

x11()
par(mfcol=c(1,1), mar=c(9,4,2,0),oma=c(1,1,1,1))
par(mgp=c(2.5,1,0))
barplot(rbind(PLest[,1]/LData*100,LNest[,1]/LData*100), las=2, beside=T, ylim=c(0,100),col=c("coral","blue"),names.arg=VarNames, ylab="Percent [%]",main="Percent [%] of right tail data used to fit the distribution",legend=c("Power Law","Lognormal"),cex.lab=1.8)

###--------------------------------------------------------------------------------------------------------------------------------------------------------


### This is the Table with Kolmogorov-Smirnov Goodness of fit and p-value to fit the data
### used in the analysis for Power Law (col1-2) and Lognormal (col3-4)
cbind(PLest[,6:7],LNest[,8:9])


### Line Plots of the Goodness of Fit for the two models
###__________________________________________
x11()
par(mfcol=c(1,1), mar=c(9,4,2,0),oma=c(1,1,1,1))
par(mgp=c(2.5,1,0))
plot(PLest[,6],type="b",col="coral",pch=15,lty=1,ylim=c(0,max(PLest[,6],LNest[,8])), xaxt="n", xlab="", ylab="KS Goodness of Fit", cex.lab=1.4, lwd=2, main="Kolmogorov-Smirnov based Goodness of Fit")
lines(LNest[,8],type="b",col="blue",pch=17,lty=2,lwd=2)
axis(1, at=1:n,labels=VarNames,las=2)
legend("topright",c("Power Law","Lognormal"),col=c("coral","blue"),lty=c(1,2),lwd=c(2,2),pch=c(15,17),bty="n")



### Line Plots of the p-values of the Fit for the two models
###___________________________________________
x11()
par(mfcol=c(1,1), mar=c(9,4,2,0),oma=c(1,1,1,1))
par(mgp=c(2.5,1,0))
plot(PLest[,7],type="b",col="coral",pch=15,lty=1,ylim=c(0,max(PLest[,7],LNest[,9])), xaxt="n", xlab="", ylab="p-values", cex.lab=1.4, lwd=2, main="P-values of the fitted model")
lines(LNest[,9],type="b",col="blue",pch=17,lty=2,lwd=2)
axis(1, at=1:n,labels=VarNames,las=2)
abline(h=0.05,col="red",lty=3,lwd=2)
legend("topleft",c("Power Law","Lognormal","0.05 p-value reference"),col=c("coral","blue","red"),lty=c(1,2,3),lwd=c(2,2,3),pch=c(15,17,NA),bty="n")


###--------------------------------------------------------------------------------------------------------------------------------------------------------

### This is the Table with the alpha point estimate (col1) and its stand. error (col2) for the Power Law fit
PLest[,4:5]

### Line Plots of the alpha values for the Power Law fitted model
###_________________________________________________
x11()
par(mfcol=c(1,1), mar=c(9,4,2,0),oma=c(1,1,1,1))
par(mgp=c(2.5,1,0))
plot(PLest[,4],type="b",col="coral",pch=15,lty=1, xaxt="n", xlab="", ylab=expression(paste(hat(alpha)[PL])), cex.lab=1.4, lwd=2, main=expression(paste("Power Law: point estimates of parameter ",alpha)),cex.main=1.5,col.main="coral")
axis(1, at=1:n,labels=VarNames,las=2)



### CI plot of the alpha values for the Power Law fitted model
###______________________________________________
eps<-0.1
critval<-0.05
zcrit<-qnorm(1-critval/2)
ylimmin<-0
ylimmax<-0
x11()
par(mfcol=c(1,1), mar=c(9,4,2,0),oma=c(1,1,1,1))
par(mgp=c(2.5,1,0))
for (i in 1:n){
	CIL<-PLest[i,4]-zcrit*PLest[i,5]
	CIU<-PLest[i,4]+zcrit*PLest[i,5]
	ylimmin<-min(ylimmin,CIL)
	ylimmax<-max(ylimmax,CIU)	
}
plot(PLest[,4],type="p",col="coral",pch=15,lty=1, xaxt="n", xlab="", ylim=c(ylimmin,ylimmax),ylab=expression(paste("Confidence Interval for ",alpha)), cex.lab=1.4, lwd=2, main=expression(paste("Power Law: approximate 95% CI for parameter ",alpha)),cex.main=1.5,col.main="coral")
axis(1, at=1:n,labels=VarNames,las=2)
for (i in 1:n){
	CIL<-PLest[i,4]-zcrit*PLest[i,5]
	CIU<-PLest[i,4]+zcrit*PLest[i,5]
	segments(i,CIL,i,CIU,col="coral",lwd=2)		### this is the line of CI
	segments(i-eps,CIL,i+eps,CIL,col="coral",lwd=2)		### this is the Lower annotation of CI
	segments(i-eps,CIU,i+eps,CIU,col="coral",lwd=2)		### this is the Upper annotation of CI	
}

###--------------------------------------------------------------------------------------------------------------------------------------------------------

### This is the Table with the mu (col1) and sigma (col3) point estimates and their stand. errors (col2 & col4) for the Lognormal fit
LNest[,4:7]

### Line Plot of the mu values for the Lognormal fitted model
###______________________________________________
x11()
par(mfcol=c(1,1), mar=c(9,4,2,0),oma=c(1,1,1,1))
par(mgp=c(2.5,1,0))
plot(LNest[,4],type="b",col="blue",pch=17,lty=2, xaxt="n", xlab="", ylab=expression(paste(hat(mu)[LN])), cex.lab=1.4, lwd=2, main=expression(paste("Lognormal: point estimates of parameter ",mu)),cex.main=1.5,col.main="blue")
axis(1, at=1:n,labels=VarNames,las=2)


### CI plot of the mu values for the Lognormal fitted model
###____________________________________________
eps<-0.1
critval<-0.05
zcrit<-qnorm(1-critval/2)
ylimmin<-0
ylimmax<-0
x11()
par(mfcol=c(1,1), mar=c(9,4,2,0),oma=c(1,1,1,1))
par(mgp=c(2.5,1,0))
for (i in 1:n){
	CIL<-LNest[i,4]-zcrit*LNest[i,5]
	CIU<-LNest[i,4]+zcrit*LNest[i,5]
	ylimmin<-min(ylimmin,CIL)
	ylimmax<-max(ylimmax,CIU)	
}
plot(LNest[,4],type="p",col="blue",pch=17,lty=1, xaxt="n", xlab="", ylim=c(ylimmin, ylimmax),ylab=expression(paste("Confidence Interval for ",mu)), cex.lab=1.4, lwd=2, main=expression(paste("Lognormal: approximate 95% CI for parameter ",mu)),cex.main=1.5,col.main="blue")
axis(1, at=1:n,labels=VarNames,las=2)
for (i in 1:n){
	CIL<-LNest[i,4]-zcrit*LNest[i,5]
	CIU<-LNest[i,4]+zcrit*LNest[i,5]
	segments(i,CIL,i,CIU,col="blue",lwd=2)		### this is the line of CI
	segments(i-eps,CIL,i+eps,CIL,col="blue",lwd=2)		### this is the Lower annotation of CI
	segments(i-eps,CIU,i+eps,CIU,col="blue",lwd=2)		### this is the Upper annotation of CI	
}

###--------------------------------------------------------------------------------------------------------------------------------------------------------


### Line Plot of the sigma values for the Lognormal fitted model
###________________________________________________
x11()
par(mfcol=c(1,1), mar=c(9,4,2,0),oma=c(1,1,1,1))
par(mgp=c(2.5,1,0))
plot(LNest[,6],type="b",col="blue",pch=17,lty=2, xaxt="n", xlab="", ylab=expression(paste(hat(sigma)[LN])), cex.lab=1.4, lwd=2, main=expression(paste("Lognormal: point estimates of parameter ",sigma)),cex.main=1.5,col.main="blue")
axis(1, at=1:n,labels=VarNames,las=2)


### CI plot of the sigma values for the Lognormal fitted model
###______________________________________________
eps<-0.1
critval<-0.05
zcrit<-qnorm(1-critval/2)
ylimmin1<-0
ylimmax1<-0
x11()
par(mfcol=c(1,1), mar=c(9,4,2,0),oma=c(1,1,1,1))
par(mgp=c(2.5,1,0))
for (i in 1:n){
	CIL<-max(LNest[i,6]-zcrit*LNest[i,7],0)
	CIU<-LNest[i,6]+zcrit*LNest[i,7]
	ylimmin1<-min(ylimmin1,CIL)
	ylimmax1<-max(ylimmax1,CIU)	
}
plot(LNest[,6],type="p",col="blue",pch=17,lty=1, xaxt="n", xlab="", ylim=c(ylimmin1, ylimmax1),ylab=expression(paste("Confidence Interval for ",sigma)), cex.lab=1.4, lwd=2, main=expression(paste("Lognormal: approximate 95% CI for parameter ",sigma)),cex.main=1.5,col.main="blue")
axis(1, at=1:n,labels=VarNames,las=2)
for (i in 1:n){
	CIL<-max(LNest[i,6]-zcrit*LNest[i,7],0)
	CIU<-LNest[i,6]+zcrit*LNest[i,7]
	segments(i,CIL,i,CIU,col="blue",lwd=2)		### this is the line of CI
	segments(i-eps,CIL,i+eps,CIL,col="blue",lwd=2)		### this is the Lower annotation of CI
	segments(i-eps,CIU,i+eps,CIU,col="blue",lwd=2)		### this is the Upper annotation of CI	
}

###--------------------------------------------------------------------------------------------------------------------------------------------------------

#dev.off() 

#dev.off() 


tocALL<-date()

print(paste("Number of iterations:",BSiter))
ticALL
tocALL

