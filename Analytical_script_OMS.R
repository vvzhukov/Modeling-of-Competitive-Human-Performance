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
data_path <- "/Users/vitalii/UH-CPL/ACEs/data_12122019"  ### This is for the MAC
setwd(data_path) ### will store figures at that folder

datasets <- c("OMS_FRA", "OMS_USA", "OMS_GBR")

tmpOMS_NOCL<-list(read.csv(paste(data_path,"/temporal_cumulative/OMS_cumulative_FRA.csv", sep=""),header=T,sep=","),
               read.csv(paste(data_path,"/temporal_cumulative/OMS_cumulative_USA.csv", sep=""),header=T,sep=","),
               read.csv(paste(data_path,"/temporal_cumulative/OMS_cumulative_GBR.csv", sep=""),header=T,sep=","))

VarNamesL<-list(c("OMS_FRA_1912", "OMS_FRA_1936", "OMS_FRA_1960", "OMS_FRA_1976", "OMS_FRA_1998", "OMS_FRA_2016"),
            c("OMS_USA_1912", "OMS_USA_1936", "OMS_USA_1960", "OMS_USA_1976", "OMS_USA_1998", "OMS_USA_2016"),
            c("OMS_GBR_1912", "OMS_GBR_1936", "OMS_GBR_1960", "OMS_GBR_1976", "OMS_GBR_1998", "OMS_GBR_2016"))

for (i in 2:length(datasets)) {

tmpOMS_NOC <- tmpOMS_NOCL[i][[1]]
VarNames <- VarNamesL[i][[1]]

OMS_NOC_cumm_time<-tmpOMS_NOC[,4]
CTV<-unique(OMS_NOC_cumm_time)		### Cummulative Time Values

### Times at which we will study the evolution:
OMS_time<-c(6,11,15,19,26,35)		### 6=1912, 11=1936, 15=1960, 19=1976, 26=1998, 35=2000

OMS_NOC_1912<-tmpOMS_NOC[tmpOMS_NOC[,4]==CTV[OMS_time[1]],2]		### USA olympic scores in 1912
OMS_NOC_1936<-tmpOMS_NOC[tmpOMS_NOC[,4]==CTV[OMS_time[2]],2]		### USA olympic scores in 1936
OMS_NOC_1960<-tmpOMS_NOC[tmpOMS_NOC[,4]==CTV[OMS_time[3]],2]		### USA olympic scores in 1960
OMS_NOC_1976<-tmpOMS_NOC[tmpOMS_NOC[,4]==CTV[OMS_time[4]],2]		### USA olympic scores in 1976
OMS_NOC_1998<-tmpOMS_NOC[tmpOMS_NOC[,4]==CTV[OMS_time[5]],2]		### USA olympic scores in 1998
OMS_NOC_2016<-tmpOMS_NOC[tmpOMS_NOC[,4]==CTV[OMS_time[6]],2]		### USA olympic scores in 2016

Data<-list(OMS_NOC_1912, OMS_NOC_1936, OMS_NOC_1960, OMS_NOC_1976, OMS_NOC_1998, OMS_NOC_2016)

n<-length(VarNames)			### This is the number of the recorded variables (i.e. columns in the list Data)


setwd(paste(data_path, "/scripts_results/", datasets[i], sep=""))

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
for (i in 1:n){
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
    
    ### 1st figure
    png(paste("2_PL_",VarNames[i],".png", sep="")) 
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
    dev.off()
    
    ### 2nd figure
    png(paste("2_LN_",VarNames[i],".png", sep=""))
    plot(bsLN_p)
    legend("bottomright",legend=paste("Lognormal \n",VarNames[i]), cex=1, bty="n")
    dev.off()
    
    ### Here we plot the two fits along with the bootstrap distribution of the parameters 			
    ### 3rd figure
    png(paste("3_PL_LN_",VarNames[i],".png", sep=""))
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
    dev.off()
    
    print("************************************************************************************")
    print(paste(VarNames[i], "is completed!!!", i/n, "data analysis completed"))
    print("************************************************************************************")
    tictoc<-date()
    print(tictoc)
}
toc<-date()
tic
save(bsLN, bsLN_p, bsPL, bsPL_p, estln, estpl, LNest, PLest, bsLN_pall, bsLNall, bsPL_pall, bsPLall, m_ln, m_pl, file="results2.RData")
toc
}