library(moments)
library(poweRlaw)
library(car) 
library(MASS) 
library(STAND)
library(nortest)
library(truncdist)
library(EnvStats)
library(igraph)
library(knitr)
library(pander)
library(ggplot2)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Requires Rstudio
source('Figure_suppl_title_cex.R')


### Folder where we have the data

path2 <- "/Users/apple/UH-CPL/ACEs/data_02052020"

#as a prototype we will use cumulative datasets
BIO_raw <- read.csv(paste("./SCHOLARS/NIH_BIO_scholars_QC_temporal_v2.csv", sep=''),
                    header = T)

CS_raw <- read.csv(paste("./SCHOLARS/NSF_CS_scholars_QC_temporal_v2.csv", sep=''),
                   header = T)

# New dates
#'started 1999 - 2003', 
#'started 2004 - 2008', 
#'started 2009 - 2013'

# BIO SCholars generations split 3
BIO_s1999 <- subset(BIO_raw, (y1996 == 0 & 
                                  y1997 == 0 & 
                                  y1998 == 0) & (y1999 != 0 | 
                                                     y2000 != 0 | 
                                                     y2001 != 0 | 
                                                     y2002 != 0 | 
                                                     y2003 != 0))[,22]

BIO_s2004 <- subset(BIO_raw, (y1996 == 0 & 
                                  y1997 == 0 & 
                                  y1998 == 0 & 
                                  y1999 == 0 & 
                                  y2000 == 0 & 
                                  y2001 == 0 & 
                                  y2002 == 0 & 
                                  y2003 == 0) & 
                        (y2004 != 0 |
                             y2005 != 0 | 
                             y2006 != 0 | 
                             y2007 != 0 | 
                             y2008 != 0))[,22]

BIO_s2009 <- subset(BIO_raw, ((y1996 == 0 & 
                                   y1997 == 0 & 
                                   y1998 == 0 & 
                                   y1999 == 0 & 
                                   y2000 == 0 & 
                                   y2001 == 0 & 
                                   y2002 == 0 & 
                                   y2003 == 0 & 
                                   y2004 == 0 & 
                                   y2005 == 0 & 
                                   y2006 == 0 & 
                                   y2007 == 0 & 
                                   y2008 == 0) &
                                  (y2009 != 0 |
                                       y2010 != 0 | 
                                       y2011 != 0 | 
                                       y2012 != 0 | 
                                       y2013 != 0)))[,22]

# CS SCholars generations split 3
CS_s1999 <- subset(CS_raw, (y1996 == 0 & 
                                y1997 == 0 & 
                                y1998 == 0) & (y1999 != 0 | 
                                                   y2000 != 0 | 
                                                   y2001 != 0 | 
                                                   y2002 != 0 | 
                                                   y2003 != 0))[,22]

CS_s2004 <- subset(CS_raw, (y1996 == 0 & 
                                y1997 == 0 & 
                                y1998 == 0 & 
                                y1999 == 0 & 
                                y2000 == 0 & 
                                y2001 == 0 & 
                                y2002 == 0 & 
                                y2003 == 0) & 
                       (y2004 != 0 |
                            y2005 != 0 | 
                            y2006 != 0 | 
                            y2007 != 0 | 
                            y2008 != 0))[,22]

CS_s2009 <- subset(CS_raw, ((y1996 == 0 & 
                                 y1997 == 0 & 
                                 y1998 == 0 & 
                                 y1999 == 0 & 
                                 y2000 == 0 & 
                                 y2001 == 0 & 
                                 y2002 == 0 & 
                                 y2003 == 0 & 
                                 y2004 == 0 & 
                                 y2005 == 0 & 
                                 y2006 == 0 & 
                                 y2007 == 0 & 
                                 y2008 == 0) &
                                (y2009 != 0 |
                                     y2010 != 0 | 
                                     y2011 != 0 | 
                                     y2012 != 0 | 
                                     y2013 != 0)))[,22]

a_bio <- list(as.integer(BIO_s1999), 
              as.integer(BIO_s2004), 
              as.integer(BIO_s2009))

a_cs <- list(as.integer(CS_s1999), 
             as.integer(CS_s2004), 
             as.integer(CS_s2009))

b_bio <- data.frame(lapply(a_bio, "length<-", max(lengths(a_bio))))
colnames(b_bio) <- c("s1999", "s2004", "s2009")

b_cs <- data.frame(lapply(a_cs, "length<-", max(lengths(a_cs))))
colnames(b_cs) <- c("s1999", "s2004", "s2009")

datasets <- c(
    "SCHOLARS_BIO_5yr_gr_shifted",
    "SCHOLARS_CS_5yr_gr_shifted"
)

tmpSCL<-list(
    b_bio,
    b_cs
)

labels <- c(
    "BIO",
    "CS"
)

VarNamesL<-list(
    c("1999 - 2003", "2004 - 2008", "2009 - 2013"),
    c("1999 - 2003", "2004 - 2008", "2009 - 2013")
)


par(mfrow=c(3,4), mar=c(2,3,1,1),oma=c(1,1,1,0))
par(mgp=c(0,1,0))

layout(matrix(c(1,2,3,4,5,6,7,8,9,10,11,12), 3, 4, byrow = TRUE), 
       widths=c(1,2.5,2.5,2.5), heights=c(1.5,3.75,3.75))


for (i in 1:length(datasets)) {
    
    setwd(paste(path2, "scripts_results", datasets[i], "HIST", sep="/"))

    results2 <- paste(paste(paste(path2, "scripts_results", datasets[i], sep="/")),"/results2.RData",sep="")
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
    } 
    
    ## LEFT COLUMN
    if (i==1 & j==1){
        plot(0,0,type="n",xaxt="n",yaxt="n",ann=F,bty="n")
        rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "darkseagreen1",border = FALSE)
        text(0,0,labels[i],cex=2.7, add=TRUE,srt = 90)
    } 
    
    if (i==2 & j==1){
        plot(0,0,type="n",xaxt="n",yaxt="n",ann=F,bty="n")
        rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "darkseagreen1",border = FALSE)
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
    legend("bottomleft",legend = rp, bty = 'n', cex = 1.1)

}
}