library(moments)
library(poweRlaw)
library(car) 
library(MASS) 
library(nortest)
library(knitr)
library(ggplot2)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Requires Rstudio
source('Figure_suppl_title_cex.R')

### Folder where we have the data
path <- "/Users/apple/UH-CPL/ACEs/data_02052020"

out_path <- "/Users/apple/UH-CPL/ACEs/code_final/"
setwd(out_path) ### will store figures at that folder


pilots_raw <- read.csv(paste(path,"/Pilots_breakdown_GER.csv", sep=''))
length(ns(pilots_raw[,4:8][,1]))

# Pilots generations
s1939 <- rowSums(subset(pilots_raw[,3:8], y1939 != 0))
s1940 <- rowSums(subset(pilots_raw[,3:8], y1940 != 0 & y1939 == 0)[,2:6])
s1941 <- rowSums(subset(pilots_raw[,3:8], y1941 != 0 & y1940 == 0 & y1939 == 0)[,3:6])
s1942 <- rowSums(subset(pilots_raw[,3:8], y1942 != 0 & y1941 == 0 & y1940 == 0 & y1939 == 0)[,4:6])
s1943 <- rowSums(subset(pilots_raw[,3:8], y1943 != 0 & y1942 == 0 & y1941 == 0 & y1940 == 0 & y1939 == 0)[,5:6])
s1944 <- subset(pilots_raw[,3:9], y1944 != 0 & y1943 == 0 & y1942 == 0 & y1941 == 0 & y1940 == 0 & y1939 == 0)[,6]

a <- list(as.integer(s1940), 
          as.integer(s1941), 
          as.integer(s1942)) 

b <- data.frame(lapply(a, "length<-", max(lengths(a))))
colnames(b) <- c("s1940", "s1941", "s1942")

datasets <- c(
    "PILOTS_GENERATIONS"
)

tmpSCL<-list(
    b
)

VarNamesL<-list(
                c("1940", "1941", "1942")
)

labels <- c(
    "LW "
)

par(mfrow=c(2,4), mar=c(2,3,1,1),oma=c(1,1,1,0))
par(mgp=c(0,1,0))


layout(matrix(c(1,2,3,4,5,6,7,8,9,10,11,12), 3, 4, byrow = TRUE), 
       widths=c(1,2.5,2.5,2.5), heights=c(1.5,3.75,3.75))

for (i in 1:length(datasets)) {
    
    results2 <- paste(paste(out_path, datasets[i], sep=""),"/results2.RData",sep="")
    print(results2)
    load(results2)
    
    tmpSC <- tmpSCL[i][[1]]
    tmpSCL[1][[1]]
    tmpSCL[2][[1]]
    VarNames <- VarNamesL[i][[1]]

    Data <- tmpSC
    
    n<-length(VarNames)			### This is the number of the recorded variables (i.e. columns in the list Data)
    
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
            FR_PL_value[k]<- sort(Data[[k]])[length(sort(Data[[k]]))-PLest[,1][k]] #Fitting range value for the Power law
        } else {
            FR_PL_value[k]<- 0
        }
        if (length(sort(Data[[k]])) != LNest[,1][k]) {
            FR_LN_value[k]<- sort(Data[[k]])[length(sort(Data[[k]]))-LNest[,1][k]] #Fitting range value for the Log Normal
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
        } 
        
        ## LEFT COLUMN
        
        
        if (i==1 & j==1){
            plot(0,0,type="n",xaxt="n",yaxt="n",ann=F,bty="n")
            rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "lightskyblue1",border = FALSE)
            text(0,0,labels[i],cex=2.7, add=TRUE,srt = 90)
        } 
        
        if (i==2 & j==1){
            plot(0,0,type="n",xaxt="n",yaxt="n",ann=F,bty="n")
            rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "lightskyblue1",border = FALSE)
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
        plot(m_pl,xlab="",ylab="", cex.main=2)
        lines(m_ln, col="red",lwd=2)
        rp = vector('expression',3)
        rp[1] = substitute(expression(italic("p")~-value == MYOTHERVALUE),
                           list(MYOTHERVALUE = format(LNest[j+1,9], digits = 2)))[2]
        rp[2] = substitute(expression(italic(n) == MYVALUE),
                           list(MYVALUE = length(x),dig=2))[2]
        legend("bottomleft",legend = rp, cex=1.5, bty = 'n')
    }
}