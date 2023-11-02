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
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Requires Rstudio
source('Figure_suppl_title_cex.R')

### Folder where we have the data
data_path <- "E:/Research/ACEs/ACEs/data_03312020"
setwd(data_path) ### will store figures at that folder
load(paste(data_path, "/scripts_results_pure/results1.RData", sep=""))


aff_cog_model_data <- read.csv("E:/Research/Data_pubmed/model_data23/model_data.csv")

####################################
### HERE WE WILL LOAD THE TOTAL DATA
####################################

## PILOTS

### Here we will load the WW2 USA pilot data 
tmpWW2_USA<-read.csv(paste(data_path,"/Pilots/WW2_USA_pilots.csv", sep=''),header=T,sep=",")
WW2_USA<-ns(tmpWW2_USA[,2])
#WW2_USA<-WW2_USA[WW2_USA>5]

### Here we will load the WW2 GERMAN pilot data 
tmpWW2_GER<-read.csv(paste(data_path,"/Pilots/WW2_GER_pilots.csv", sep=''),header=T,sep=",")
WW2_GER<-ns(tmpWW2_GER[,2])
WW2_GER<-WW2_GER[WW2_GER>5]

### Here we will load the WW2 GBR pilot data 
tmpWW2_RAF<-read.csv(paste(data_path,"/Pilots/WW2_RAF_pilots.csv", sep=''),header=T,sep=",")
WW2_RAF<-ns(tmpWW2_RAF[,2])
WW2_RAF<-WW2_RAF[WW2_RAF>5]

## SCHOLARS

### Here we will load the NSF CS scholar data 
tmpNSF_CS<-read.csv(paste(data_path,"/Scholars/NSF_CS_scholars_QC2.csv", sep=''),header=T,sep=",")
NSF_CS<-ns(tmpNSF_CS[,1])
NSF_CS<-NSF_CS[NSF_CS>1]

### Here we will load the NIH BIO scholar data 
tmpNIH_BIO<-read.csv(paste(data_path,"/Scholars/NIH_BIO_scholars_QC2.csv", sep=''),header=T,sep=",")
NIH_BIO<-ns(tmpNIH_BIO[,1])
NIH_BIO<-NIH_BIO[NIH_BIO>1]


### Here we will load the PubMed Aff 
tmpAff<-subset(aff_cog_model_data, pType == 'Affective')
PUB_AFF<-ns(tmpAff$pCitations_n)
PUB_AFF<-PUB_AFF[PUB_AFF>1]


### Here we will load the PubMed Cog
tmpCog<-subset(aff_cog_model_data, pType == 'Cognitive')
PUB_COG<-ns(tmpCog$pCitations_n)
PUB_COG<-PUB_COG[PUB_COG>1]


## ATHLETES

### Here we will load the Olympic Medals Scores for USA athletes 
tmpOMS_USA<-read.csv(paste(data_path,"/Athletes/OMS_total_USA.csv", sep=''),header=T,sep=",")
tmpOMS_USA<-subset(tmpOMS_USA,total_medals>1)
OMS_USA<-tmpOMS_USA[,3]

### Here we will load the Olympic Medals Scores for FRA athletes 
tmpOMS_FRA<-read.csv(paste(data_path,"/Athletes/OMS_total_FRA.csv", sep=''),header=T,sep=",")
tmpOMS_FRA<-subset(tmpOMS_FRA,total_medals>1)
OMS_FRA<-tmpOMS_FRA[,3]


### Here we will load the Olympic Medals Scores for GBR athletes 
tmpOMS_GBR<-read.csv(paste(data_path,"/Athletes/OMS_total_GBR.csv", sep=''),header=T,sep=",")
tmpOMS_GBR<-subset(tmpOMS_GBR,total_medals>1)
OMS_GBR<-tmpOMS_GBR[,3]

### Here we will load the Olympic Medals Scores for USA swimmers athletes 
tmpOMS_USA_SW<-read.csv(paste(data_path,"/Athletes/OMS_total_USA_sw.csv", sep=''),header=T,sep=",")
tmpOMS_USA_SW<-subset(tmpOMS_USA_SW,total_records>1)
OMS_USA_SW<-tmpOMS_USA_SW[,3]

### Here we will load the Olympic Medals Scores for FRA fencing athletes 
tmpOMS_FRA_F<-read.csv(paste(data_path,"/Athletes/OMS_total_FRA_fencing.csv", sep=''),header=T,sep=",")
tmpOMS_FRA_F<-subset(tmpOMS_FRA_F,total_medals>1)
OMS_FRA_F<-tmpOMS_FRA_F[,3]


Data<-list(#WW2_USA, 
           WW2_GER, 
           #WW2_RAF, 
           NSF_CS, 
           NIH_BIO,
           PUB_AFF,
           PUB_COG,
           OMS_USA_SW,
           #OMS_USA, 
           #OMS_FRA,
           OMS_FRA_F)
#OMS_GBR

VarNames<-c(#"USAF",
            "LW", 
            #"RAF",
            "CS",
            "BIO",
            "AFF",
            "COG",
            #"USOT",
            "USOT_S",
            "FRAOT_F") 
#"OMS GBR"


n<-length(VarNames)			### This is the number of the recorded variables (i.e. columns in the list Data)

par(mfcol=c(n+1,n+1), mar=c(1,1,1,1),oma=c(1,1,1,1))
par(mgp=c(0,1,0))
plot(0,0,type="n",xaxt="n",yaxt="n",ann=F,bty="n")
text(0,0,"",cex=1.7)

for (i in 1:n){
    if (i<= 1){
        plot(0,0,type="n",xaxt="n",yaxt="n",ann=F,bty="n")
        rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "lightskyblue1",border = FALSE)
        text(0,0,VarNames[i],cex=1.7, add=TRUE)
    } 
    else if (i>1 & i<6){
        plot(0,0,type="n",xaxt="n",yaxt="n",ann=F,bty="n")
        rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "darkseagreen1",border = FALSE)
        text(0,0,VarNames[i],cex=1.7, add=TRUE)
    }
    else {
        plot(0,0,type="n",xaxt="n",yaxt="n",ann=F,bty="n")
        rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gold1",border = FALSE)
        text(0,0,VarNames[i],cex=1.7)        
    }
    #plot(0,0,type="n",xaxt="n",yaxt="n",ann=F,bty="n")
    #text(0,0,VarNames[i],cex=1.7)
}

for (i in 1:n){
    plot(0,0,type="n",xaxt="n",yaxt="n",ann=F,bty="n")
    if (i<= 1){
        rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "lightskyblue1",border = FALSE)
        text(0,0,VarNames[i],cex=1.7, add=TRUE)
    } 
    else if (i>1 & i<6){
        rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "darkseagreen1",border = FALSE)
        text(0,0,VarNames[i],cex=1.7, add=TRUE)
    }
    else {
        rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gold1",border = FALSE)
        text(0,0,VarNames[i],cex=1.7)        
    }

    for (j in 1:n){
        if (i==j){	### Here we will plot the Histogram and the density curve over histogram
            tmpData<-Data[[i]]
            print(VarNames[i])
            print(summary(Data[[i]]))
            print(sd(Data[[i]]))
            #print(skewness(Data[[i]]))
            print(length(Data[[i]]))
            #print(mode())
            hist(tmpData,freq=F,xlab="",ylab="",main="")
            # legend("topright",paste(VarNames[i]),cex=1,bty="n")
            lines(density(tmpData),col="blue",lwd=2)			
        }
        else if (i<j){	### Here we will plot the qq-plot and the regression line
            tmpDataY<-Data[[i]]
            tmpDataX<-Data[[j]]
            tmpplot<-qqplot(tmpDataX,tmpDataY,xlab="",ylab="")
            abline(lm(tmpplot$y~tmpplot$x),col="red",lwd=2)
        }
        else if (i>j){	### Here we will print the correlation coefficient of the qq-plot
            tmpDataY<-Data[[i]]
            tmpDataX<-Data[[j]]
            tmpplot<-qqplot(tmpDataX,tmpDataY,xlab="",ylab="",plot=F)
            tmpcorr<-cor(tmpplot$x,tmpplot$y)
            plot(0,0,type="n",xaxt="n",yaxt="n",ann=F)
            text(0,0,paste("r =",round(tmpcorr,3)),cex=1.5)
        }
    }
}

