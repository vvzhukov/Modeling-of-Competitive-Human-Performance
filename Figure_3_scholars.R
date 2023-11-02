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
### Folder where we have the data

path2 <- "/Users/apple/UH-CPL/ACEs/data_02052020"

#as a prototype we will use cumulative datasets
BIO_raw <- read.csv(paste("/Users/apple/Downloads/Data-5/Scholars/NIH_BIO_scholars_QC_temporal_v2.csv", sep=''),
                    header = T)
CS_raw <- read.csv(paste("/Users/apple/Downloads/Data-5/Scholars/NSF_CS_scholars_QC_temporal_v2.csv", sep=''),
                   header = T)

# New dates
#'started 1996 - 2000', 
#'started 2001 - 2005', 
#'started 2006 - 2010'

# Pilots generations
BIO_s1996 <- subset(BIO_raw, y1996 != 0 | y1997 != 0 | y1998 != 0 | 
                        y1999 != 0 | y2000 != 0)[,22]

CS_s1996 <- subset(CS_raw, y1996 != 0 | y1997 != 0 | y1998 != 0 | 
                       y1999 != 0 | y2000 != 0)[,22]

BIO_s2001 <- subset(BIO_raw, (y1996 == 0 & 
                                  y1997 == 0 & 
                                  y1998 == 0 & 
                                  y1999 == 0 & 
                                  y2000 == 0) & 
                        (y2001 != 0 |
                             y2002 != 0 | 
                             y2003 != 0 |
                             y2004 != 0 |
                             y2005 != 0))[,22]

CS_s2001 <- subset(CS_raw, (y1996 == 0 & 
                                y1997 == 0 & 
                                y1998 == 0 & 
                                y1999 == 0 & 
                                y2000 == 0) & 
                       (y2001 != 0 |
                            y2002 != 0 | 
                            y2003 != 0 |
                            y2004 != 0 |
                            y2005 != 0))[,22]


BIO_s2006 <- subset(BIO_raw, ((y1996 == 0 & 
                                   y1997 == 0 & 
                                   y1998 == 0 & 
                                   y1999 == 0 & 
                                   y2000 == 0 & 
                                   y2001 == 0 & 
                                   y2002 == 0) & 
                                  (y2003 == 0 &
                                       y2004 == 0 & 
                                       y2005 == 0)) &
                        (y2006 != 0 |
                             y2007 != 0 | 
                             y2008 != 0 |
                             y2009 != 0 |
                             y2010 != 0)
)[,22]

CS_s2006 <- subset(CS_raw, ((y1996 == 0 & 
                                 y1997 == 0 & 
                                 y1998 == 0 & 
                                 y1999 == 0 & 
                                 y2000 == 0 & 
                                 y2001 == 0 & 
                                 y2002 == 0) & 
                                (y2003 == 0 &
                                     y2004 == 0 & 
                                     y2005 == 0)) &
                       (y2006 != 0 |
                            y2007 != 0 | 
                            y2008 != 0 |
                            y2009 != 0 |
                            y2010 != 0)
)[,22]

a_bio <- list(as.integer(BIO_s1996), 
              as.integer(BIO_s2001), 
              as.integer(BIO_s2006))

a_cs <- list(as.integer(CS_s1996), 
             as.integer(CS_s2001), 
             as.integer(CS_s2006))

b_bio <- data.frame(lapply(a_bio, "length<-", max(lengths(a_bio))))
colnames(b_bio) <- c("s1996", "s2001", "s2006")

b_cs <- data.frame(lapply(a_cs, "length<-", max(lengths(a_cs))))
colnames(b_cs) <- c("s1996", "s2001", "s2006")

datasets <- c(
    "SCHOLARS_BIO_3gr", 
    "SCHOLARS_CS_3gr"
)

labels <- c(
    "BIO",
    "CS"
)

tmpSCL<-list(
    b_bio,
    b_cs
)

VarNamesL<-list(c("1996-2000", "2001-2005", "2006-2010"),
                c("1996-2000", "2001-2005", "2006-2010")
)

ns <- function (data){
    data <- na.omit(data[data>0])
}

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
    #lines(m_pl, col="blue",lwd=2)
    #plot(m_ln,xlab="",ylab="")
    lines(m_ln, col="red",lwd=2)
    #	legend("bottomleft",title=paste(VarNames[i], "\n Lognormal"),title.col="blue",c(paste("n-tail =", round(LNest[i,1],3)),paste("GoF =", round(LNest[i,8],3)),paste("p-value =", round(LNest[i,9],3))), bty="n")
    rp = vector('expression',3)
    #rp[1] = substitute(expression(italic("PL (blue) p")~-value == MYOTHERVALUE),
    #                   list(MYOTHERVALUE = format(PLest[j,7], digits = 2)))[2]
    #rp[2] = paste("[",round(LNest[j,1]/length(ns(Data[[j]]))*100,1),"% ]")
    rp[1] = substitute(expression(italic("p")~-value == MYOTHERVALUE),
                       list(MYOTHERVALUE = format(LNest[j,9], digits = 2)))[2]
    rp[2] = substitute(expression(italic(n) == MYVALUE),
                       list(MYVALUE = length(x),dig=2))[2]
    #rp[4] = substitute(expression(italic(p)~-value == MYOTHERVALUE),
    #                   list(MYOTHERVALUE = format(PLest[j,9], digits = 2)))[2]
    #rp[2] = paste(VarNames[j])
    legend("bottomleft",legend = rp, bty = 'n', cex = 1.1)
    #text("bottomleft","TEST",cex=2.7, add=TRUE)


}
}


## Here is a function wich allows to use title.cex
legend  <- function (x, y = NULL, legend, fill = NULL, col = par("col"), 
                     border = "black", lty, lwd, pch, angle = 45, density = NULL, 
                     bty = "o", bg = par("bg"), box.lwd = par("lwd"), box.lty = par("lty"), 
                     box.col = par("fg"), pt.bg = NA, cex = 1, pt.cex = cex, pt.lwd = lwd, 
                     xjust = 0, yjust = 1, x.intersp = 1, y.intersp = 1, adj = c(0, 
                                                                                 0.5), text.width = NULL, text.col = par("col"), text.font = NULL, 
                     merge = do.lines && has.pch, trace = FALSE, plot = TRUE, 
                     ncol = 1, horiz = FALSE, title = NULL, inset = 0, xpd, title.cex = cex,title.col = text.col, 
                     title.adj = 0.5, seg.len = 2) 
{
    if (missing(legend) && !missing(y) && (is.character(y) || 
                                           is.expression(y))) {
        legend <- y
        y <- NULL
    }
    mfill <- !missing(fill) || !missing(density)
    if (!missing(xpd)) {
        op <- par("xpd")
        on.exit(par(xpd = op))
        par(xpd = xpd)
    }
    title <- as.graphicsAnnot(title)
    if (length(title) > 1)
        stop("invalid 'title'")
    legend <- as.graphicsAnnot(legend)
    n.leg <- if (is.call(legend))
        1
    else length(legend)
    if (n.leg == 0)
        stop("'legend' is of length 0")
    auto <- if (is.character(x))
        match.arg(x, c("bottomright", "bottom", "bottomleft",
                       "left", "topleft", "top", "topright", "right", "center"))
    else NA
    if (is.na(auto)) {
        xy <- xy.coords(x, y)
        x <- xy$x
        y <- xy$y
        nx <- length(x)
        if (nx < 1 || nx > 2) 
            stop("invalid coordinate lengths")
    }
    else nx <- 0
    xlog <- par("xlog")
    ylog <- par("ylog")
    rect2 <- function(left, top, dx, dy, density = NULL, angle, 
                      ...) {
        r <- left + dx
        if (xlog) {
            left <- 10^left
            r <- 10^r
        }
        b <- top - dy
        if (ylog) {
            top <- 10^top
            b <- 10^b
        }
        rect(left, top, r, b, angle = angle, density = density, 
             ...)
    }
    segments2 <- function(x1, y1, dx, dy, ...) {
        x2 <- x1 + dx
        if (xlog) {
            x1 <- 10^x1
            x2 <- 10^x2
        }
        y2 <- y1 + dy
        if (ylog) {
            y1 <- 10^y1
            y2 <- 10^y2
        }
        segments(x1, y1, x2, y2, ...)
    }
    points2 <- function(x, y, ...) {
        if (xlog) 
            x <- 10^x
        if (ylog) 
            y <- 10^y
        points(x, y, ...)
    }
    text2 <- function(x, y, ...) {
        if (xlog) 
            x <- 10^x
        if (ylog) 
            y <- 10^y
        text(x, y, ...)
    }
    if (trace) 
        catn <- function(...) do.call("cat", c(lapply(list(...), 
                                                      formatC), list("\n")))
    cin <- par("cin")
    Cex <- cex * par("cex")
    if (is.null(text.width)) 
        text.width <- max(abs(strwidth(legend, units = "user", 
                                       cex = cex, font = text.font)))
    else if (!is.numeric(text.width) || text.width < 0) 
        stop("'text.width' must be numeric, >= 0")
    xc <- Cex * xinch(cin[1L], warn.log = FALSE)
    yc <- Cex * yinch(cin[2L], warn.log = FALSE)
    if (xc < 0) 
        text.width <- -text.width
    xchar <- xc
    xextra <- 0
    yextra <- yc * (y.intersp - 1)
    ymax <- yc * max(1, strheight(legend, units = "user", cex = cex)/yc)
    ychar <- yextra + ymax
    if (trace) 
        catn("  xchar=", xchar, "; (yextra,ychar)=", c(yextra, 
                                                       ychar))
    if (mfill) {
        xbox <- xc * 0.8
        ybox <- yc * 0.5
        dx.fill <- xbox
    }
    do.lines <- (!missing(lty) && (is.character(lty) || any(lty > 
                                                                0))) || !missing(lwd)
    n.legpercol <- if (horiz) {
        if (ncol != 1) 
            warning(gettextf("horizontal specification overrides: Number of columns := %d", 
                             n.leg), domain = NA)
        ncol <- n.leg
        1
    }
    else ceiling(n.leg/ncol)
    has.pch <- !missing(pch) && length(pch) > 0
    if (do.lines) {
        x.off <- if (merge) 
            -0.7
        else 0
    }
    else if (merge) 
        warning("'merge = TRUE' has no effect when no line segments are drawn")
    if (has.pch) {
        if (is.character(pch) && !is.na(pch[1L]) && nchar(pch[1L], 
                                                          type = "c") > 1) {
            if (length(pch) > 1) 
                warning("not using pch[2..] since pch[1L] has multiple chars")
            np <- nchar(pch[1L], type = "c")
            pch <- substr(rep.int(pch[1L], np), 1L:np, 1L:np)
        }
        if (!is.character(pch)) 
            pch <- as.integer(pch)
    }
    if (is.na(auto)) {
        if (xlog) 
            x <- log10(x)
        if (ylog) 
            y <- log10(y)
    }
    if (nx == 2) {
        x <- sort(x)
        y <- sort(y)
        left <- x[1L]
        top <- y[2L]
        w <- diff(x)
        h <- diff(y)
        w0 <- w/ncol
        x <- mean(x)
        y <- mean(y)
        if (missing(xjust)) 
            xjust <- 0.5
        if (missing(yjust)) 
            yjust <- 0.5
    }
    else {
        h <- (n.legpercol + (!is.null(title))) * ychar + yc
        w0 <- text.width + (x.intersp + 1) * xchar
        if (mfill) 
            w0 <- w0 + dx.fill
        if (do.lines) 
            w0 <- w0 + (seg.len + x.off) * xchar
        w <- ncol * w0 + 0.5 * xchar
        if (!is.null(title) && (abs(tw <- strwidth(title, units = "user", 
                                                   cex = cex) + 0.5 * xchar)) > abs(w)) {
            xextra <- (tw - w)/2
            w <- tw
        }
        if (is.na(auto)) {
            left <- x - xjust * w
            top <- y + (1 - yjust) * h
        }
        else {
            usr <- par("usr")
            inset <- rep_len(inset, 2)
            insetx <- inset[1L] * (usr[2L] - usr[1L])
            left <- switch(auto, bottomright = , topright = , 
                           right = usr[2L] - w - insetx, bottomleft = , 
                           left = , topleft = usr[1L] + insetx, bottom = , 
                           top = , center = (usr[1L] + usr[2L] - w)/2)
            insety <- inset[2L] * (usr[4L] - usr[3L])
            top <- switch(auto, bottomright = , bottom = , bottomleft = usr[3L] + 
                              h + insety, topleft = , top = , topright = usr[4L] - 
                              insety, left = , right = , center = (usr[3L] + 
                                                                       usr[4L] + h)/2)
        }
    }
    if (plot && bty != "n") {
        if (trace) 
            catn("  rect2(", left, ",", top, ", w=", w, ", h=", 
                 h, ", ...)", sep = "")
        rect2(left, top, dx = w, dy = h, col = bg, density = NULL, 
              lwd = box.lwd, lty = box.lty, border = box.col)
    }
    xt <- left + xchar + xextra + (w0 * rep.int(0:(ncol - 1), 
                                                rep.int(n.legpercol, ncol)))[1L:n.leg]
    yt <- top - 0.5 * yextra - ymax - (rep.int(1L:n.legpercol, 
                                               ncol)[1L:n.leg] - 1 + (!is.null(title))) * ychar
    if (mfill) {
        if (plot) {
            if (!is.null(fill)) 
                fill <- rep_len(fill, n.leg)
            rect2(left = xt, top = yt + ybox/2, dx = xbox, dy = ybox, 
                  col = fill, density = density, angle = angle, 
                  border = border)
        }
        xt <- xt + dx.fill
    }
    if (plot && (has.pch || do.lines)) 
        col <- rep_len(col, n.leg)
    if (missing(lwd) || is.null(lwd)) 
        lwd <- par("lwd")
    if (do.lines) {
        if (missing(lty) || is.null(lty)) 
            lty <- 1
        lty <- rep_len(lty, n.leg)
        lwd <- rep_len(lwd, n.leg)
        ok.l <- !is.na(lty) & (is.character(lty) | lty > 0) & 
            !is.na(lwd)
        if (trace) 
            catn("  segments2(", xt[ok.l] + x.off * xchar, ",", 
                 yt[ok.l], ", dx=", seg.len * xchar, ", dy=0, ...)")
        if (plot) 
            segments2(xt[ok.l] + x.off * xchar, yt[ok.l], dx = seg.len * 
                          xchar, dy = 0, lty = lty[ok.l], lwd = lwd[ok.l], 
                      col = col[ok.l])
        xt <- xt + (seg.len + x.off) * xchar
    }
    if (has.pch) {
        pch <- rep_len(pch, n.leg)
        pt.bg <- rep_len(pt.bg, n.leg)
        pt.cex <- rep_len(pt.cex, n.leg)
        pt.lwd <- rep_len(pt.lwd, n.leg)
        ok <- !is.na(pch)
        if (!is.character(pch)) {
            ok <- ok & (pch >= 0 | pch <= -32)
        }
        else {
            ok <- ok & nzchar(pch)
        }
        x1 <- (if (merge && do.lines) 
            xt - (seg.len/2) * xchar
            else xt)[ok]
        y1 <- yt[ok]
        if (trace) 
            catn("  points2(", x1, ",", y1, ", pch=", pch[ok], 
                 ", ...)")
        if (plot) 
            points2(x1, y1, pch = pch[ok], col = col[ok], cex = pt.cex[ok], 
                    bg = pt.bg[ok], lwd = pt.lwd[ok])
    }
    xt <- xt + x.intersp * xchar
    if (plot) {
        if (!is.null(title)) 
            text2(left + w * title.adj, top - ymax, labels = title, 
                  adj = c(title.adj, 0), cex = title.cex, col = title.col)
        text2(xt, yt, labels = legend, adj = adj, cex = cex, 
              col = text.col, font = text.font)
    }
    invisible(list(rect = list(w = w, h = h, left = left, top = top), 
                   text = list(x = xt, y = yt)))
}