library(moments)
library(poweRlaw)
library(car) 
library(nortest)
library(EnvStats)
library(knitr)
library(ggplot2)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Requires Rstudio
source('Figure_suppl_title_cex.R')

### Folder where we have the data
data_path <- "/Users/apple/UH-CPL/ACEs/data_02052020/scripts_results/" ## Store results
setwd(data_path) ### will store figures at that folder

load(paste(paste(data_path, "ATHLETES_USA_SWIMMERS_4_GENS", sep=""),"/results2.RData",sep=""))

datasets <- c(
    "ATHLETES_USA_SWIMMERS_4_GENS"
)

labels <- c(
    "USOT_S"
)


# Allign plots
par(mfrow=c(3,5), mar=c(2,3,1,1),oma=c(1,1,1,0))
par(mgp=c(0,1,0))

layout(matrix(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15), 3, 5, byrow = TRUE), 
       widths=c(1,2,2,2,2), heights=c(1,2.5,2.5))


for (i in 1:length(datasets)) {
    
    setwd(paste(data_path, datasets[i], "/HIST", sep=""))

    results2 <- paste(paste(data_path, datasets[i], sep=""),"/results2.RData",sep="")
    print(results2)
    load(results2)
    
    
    tmpSC <- tmpSCL[i][[1]]
    tmpSCL[1][[1]]
    tmpSCL[2][[1]]
    VarNames <- VarNamesL[i][[1]]
    
    Data <- tmpSC
    
    
    n<-length(VarNames)			
    ### This is the number of the recorded variables (i.e. columns in the list Data)
    
    LData<-rep(NA, n)
    FR_PL_value<-rep(NA, n)
    FR_LN_value<-rep(NA, n)
    MData<-rep(NA, n)
    ### In this vector we will put the minimum integer value of each of the data sets
    MinData<-rep(NA, n)
    
    for (k in 1:n){
        LData[k]<-length(ns(Data[[k]]))
        MData[k]<-max(ns(Data[k]))
        MinData[k]<-min(Data[[k]])
        if (length(sort(Data[[k]])) != PLest[,1][k]) {
            FR_PL_value[k]<- sort(Data[[k]])[length(sort(Data[[k]]))-PLest[,1][k]] 
            #Fitting range value for the Power law
        } else {
            FR_PL_value[k]<- 0
        }
        if (length(sort(Data[[k]])) != LNest[,1][k]) {
            FR_LN_value[k]<- sort(Data[[k]])[length(sort(Data[[k]]))-LNest[,1][k]] 
            #Fitting range value for the Log Normal
        } else {
            FR_LN_value[k]<- 0
        }
    }
    
    for (j in 1:n){
        
        
        ## TOP ROW
        if (i==1 & j==1){
            plot(0,0,type="n",xaxt="n",yaxt="n",ann=F,bty="n")
            rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "white",border = FALSE)
            
            plot(0,0,type="n",xaxt="n",yaxt="n",ann=F,bty="n")
            rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray90",border = FALSE)
            text(0,0,VarNames[i],cex=2.7, add=TRUE)
            
            plot(0,0,type="n",xaxt="n",yaxt="n",ann=F,bty="n")
            rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray90",border = FALSE)
            text(0,0,VarNames[i+1],cex=2.7, add=TRUE)
            
            plot(0,0,type="n",xaxt="n",yaxt="n",ann=F,bty="n")
            rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray90",border = FALSE)
            text(0,0,VarNames[i+2],cex=2.7, add=TRUE)
            
            plot(0,0,type="n",xaxt="n",yaxt="n",ann=F,bty="n")
            rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray90",border = FALSE)
            text(0,0,VarNames[i+3],cex=2.7, add=TRUE)
        } 
        
        ## LEFT COLUMN
        if (i==1 & j==1){
            plot(0,0,type="n",xaxt="n",yaxt="n",ann=F,bty="n")
            rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gold1",border = FALSE)
            text(0,0,labels[i],cex=2.7, add=TRUE,srt = 90)
        } 
        
        x<-Data[[j]]
        x<-ns(x)
        
        ### Here we fit the Lognormal
        m_ln <- dislnorm$new(x)
        estln <- estimate_xmin(m_ln)
        m_ln$setXmin(estln)
        m_pl <- displ$new(x)
        estpl <- estimate_xmin(m_pl)
        m_pl$setXmin(estpl)
        plot(m_pl,xlab="",ylab="")
        lines(m_ln, col="red",lwd=2)
        rp = vector('expression',3)
        rp[1] = substitute(expression(italic("p")~-value == MYOTHERVALUE),
                           list(MYOTHERVALUE = format(LNest[j,9], digits = 2)))[2]
        rp[2] = substitute(expression(italic(n) == MYVALUE),
                           list(MYVALUE = length(x),dig=2))[2]
        legend("bottomleft",legend = rp, cex=1.1, bty = 'n')
    }
}

### Folder where we have the data
data_path <- "/Users/apple/UH-CPL/ACEs/data_02052020/scripts_results/" ## Store results
setwd(data_path) ### will store figures at that folder

load(paste(paste(data_path, "ATHLETES_FRA_FENCING_2_GR", sep=""),"/results2.RData",sep=""))

datasets <- c(
    "ATHLETES_FRA_FENCING_2_GR",
    "ATHLETES_FRA_FENCING_4_GR"
)

labels <- c(
    "FRAOT_F",
    "FRAOT_F",
    "FRAOT_F",
    "FRAOT_F"
)

for (i in 2:length(datasets)) {
    
    setwd(paste(data_path, datasets[i], "/HIST", sep=""))
    
    results2 <- paste(paste(data_path, datasets[i], sep=""),"/results2.RData",sep="")
    print(results2)
    load(results2)
    
    tmpSC <- tmpSCL[i][[1]]
    tmpSCL[1][[1]]
    tmpSCL[2][[1]]
    VarNames <- VarNamesL[i][[1]]
    Data <- tmpSC
    n<-length(VarNames)			
    ### This is the number of the recorded variables (i.e. columns in the list Data)
    
    LData<-rep(NA, n)
    FR_PL_value<-rep(NA, n)
    FR_LN_value<-rep(NA, n)
    MData<-rep(NA, n)
    ### In this vector we will put the minimum integer value of each of the data sets
    MinData<-rep(NA, n)
    
    for (k in 1:n){
        LData[k]<-length(ns(Data[[k]]))
        MData[k]<-max(ns(Data[k]))
        MinData[k]<-min(Data[[k]])
        if (length(sort(Data[[k]])) != PLest[,1][k]) {
            FR_PL_value[k]<- sort(Data[[k]])[length(sort(Data[[k]]))-PLest[,1][k]] 
            #Fitting range value for the Power law
        } else {
            FR_PL_value[k]<- 0
        }
        if (length(sort(Data[[k]])) != LNest[,1][k]) {
            FR_LN_value[k]<- sort(Data[[k]])[length(sort(Data[[k]]))-LNest[,1][k]] 
            #Fitting range value for the Log Normal
        } else {
            FR_LN_value[k]<- 0
        }
    }
    
    for (j in 1:n){
        
        
        if (i==2 & j==1){
            plot(0,0,type="n",xaxt="n",yaxt="n",ann=F,bty="n")
            rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gold1",border = FALSE)
            text(0,0,labels[i],cex=2.7, add=TRUE,srt=90)
        } 
        
        x<-Data[[j]]
        x<-ns(x)
        ### Here we fit the Lognormal
        m_ln <- dislnorm$new(x)
        estln <- estimate_xmin(m_ln)
        m_ln$setXmin(estln)
        
        m_pl <- displ$new(x)
        estpl <- estimate_xmin(m_pl)
        m_pl$setXmin(estpl)
        plot(m_pl,xlab="",ylab="")
        #lines(m_pl, col="blue",lwd=2)
        lines(m_ln, col="red",lwd=2)
        rp = vector('expression',3)
        #rp[1] = substitute(expression(italic("PL p")~-value == MYOTHERVALUE),
        #                   list(MYOTHERVALUE = format(PLest[j,7], digits = 2)))[2]
        rp[1] = substitute(expression(italic("p")~-value == MYOTHERVALUE),
                           list(MYOTHERVALUE = format(LNest[j,9], digits = 2)))[2]
        rp[2] = substitute(expression(italic(n) == MYVALUE),
                           list(MYVALUE = length(x),dig=2))[2]
        legend("bottomleft",legend = rp, cex=1.1, bty = 'n')
    }
}