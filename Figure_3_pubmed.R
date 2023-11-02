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


path2 <- "E:/Research/ACEs/ACEs/data_02052020"


aff_cog_model_data <- read.csv("E:/Research/Data_pubmed/model_data23/model_data.csv")



### Here we will load the PubMed Aff 
tmpAff<-subset(aff_cog_model_data, pType == 'Affective')
PUB_AFF<-ns(tmpAff$pCitations_n)
PUB_AFF<-PUB_AFF[PUB_AFF>1]


### Here we will load the PubMed Cog
tmpCog<-subset(aff_cog_model_data, pType == 'Cognitive')
PUB_COG<-ns(tmpCog$pCitations_n)
PUB_COG<-PUB_COG[PUB_COG>1]


# New dates
#'started 1968 - 1990', 
#'started 1991 - 2000', 
#'started 2001 - 2010'
#'started 2011 - 2022'


AFF_s1968 <- subset(tmpAff, pYear %in% c(1968:1990))$pCitations_n
AFF_s1991 <- subset(tmpAff, pYear %in% c(1991:2000))$pCitations_n
AFF_s2001 <- subset(tmpAff, pYear %in% c(2001:2010))$pCitations_n
AFF_s2011 <- subset(tmpAff, pYear %in% c(2011:2022))$pCitations_n

COG_s1968 <- subset(tmpCog, pYear %in% c(1968:1990))$pCitations_n
COG_s1991 <- subset(tmpCog, pYear %in% c(1991:2000))$pCitations_n
COG_s2001 <- subset(tmpCog, pYear %in% c(2001:2010))$pCitations_n
COG_s2011 <- subset(tmpCog, pYear %in% c(2011:2022))$pCitations_n


a_bio <- list(as.integer(AFF_s1968), 
              as.integer(AFF_s1991), 
              as.integer(AFF_s2001), 
              as.integer(AFF_s2011))

a_cs <- list(as.integer(COG_s1968), 
             as.integer(COG_s1991), 
             as.integer(COG_s2001), 
             as.integer(COG_s2011))

b_bio <- data.frame(lapply(a_bio, "length<-", max(lengths(a_bio))))
colnames(b_bio) <- c("s1968", "s1991", "s2001", "s2011")

b_cs <- data.frame(lapply(a_cs, "length<-", max(lengths(a_cs))))
colnames(b_cs) <- c("s1968", "s1991", "s2001", "s2011")

datasets <- c(
  "PUBMED_AFF_4_frames",
  "PUBMED_COG_4_frames"
)

tmpSCL<-list(
  b_bio,
  b_cs
)

VarNamesL<-list(
  c("s1968", "s1991", "s2001", "s2011"),
  c("s1968", "s1991", "s2001", "s2011")
)



par(mfrow=c(3,5), mar=c(2,3,1,1),oma=c(1,1,1,0))
par(mgp=c(0,1,0))

layout(matrix(c(1:15), 3, 5, byrow = TRUE), 
       widths=c(1,2.5,2.5,2.5,2.5), heights=c(1.5,3.75,3.75))


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
        
        plot(0,0,type="n",xaxt="n",yaxt="n",ann=F,bty="n")
        rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray90",border = FALSE)
        text(0,0,VarNames[i+3],cex=2.7, add=TRUE)
    } 
    
    ## LEFT COLUMN
    if (i==1 & j==1){
        plot(0,0,type="n",xaxt="n",yaxt="n",ann=F,bty="n")
        rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "darkseagreen1",border = FALSE)
        text(0,0,"Affective",cex=2.7, add=TRUE,srt = 90)
    } 
    
    if (i==2 & j==1){
        plot(0,0,type="n",xaxt="n",yaxt="n",ann=F,bty="n")
        rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "darkseagreen1",border = FALSE)
        text(0,0,"Cognitive",cex=2.7, add=TRUE,srt = 90)
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
    #rp[2] = substitute(expression(italic(n) == MYVALUE),
    #                   list(MYVALUE = length(x),dig=2))[2]
    legend("bottomleft",legend = rp, bty = 'n', cex = 1.1)

}
}