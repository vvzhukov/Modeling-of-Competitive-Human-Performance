library(car)

### Folder where we have the data
path <- "/Users/apple/UH-CPL/ACEs/data_02052020"
pilots_raw <- read.csv(paste(path,"/Pilots_breakdown_GER.csv", sep=''))


# Pilots generations
s1939 <- rowSums(subset(pilots_raw[,3:8], y1939 != 0))
s1940 <- rowSums(subset(pilots_raw[,3:8], y1940 != 0 & y1939 == 0)[,2:6])
s1941 <- rowSums(subset(pilots_raw[,3:8], y1941 != 0 & y1940 == 0 & y1939 == 0)[,3:6])
s1942 <- rowSums(subset(pilots_raw[,3:8], y1942 != 0 & y1941 == 0 & y1940 == 0 & y1939 == 0)[,4:6])
s1943 <- rowSums(subset(pilots_raw[,3:8], y1943 != 0 & y1942 == 0 & y1941 == 0 & y1940 == 0 & y1939 == 0)[,5:6])
s1944 <- subset(pilots_raw[,3:9], y1944 != 0 & y1943 == 0 & y1942 == 0 & y1941 == 0 & y1940 == 0 & y1939 == 0)[,6]


data_pilots <- rbind(data.frame("victories"=s1939, "year" = "1939"), 
                    data.frame("victories"=s1940, "year" = "1940"),
                    data.frame("victories"=s1941, "year" = "1941"),
                    data.frame("victories"=s1942, "year" = "1942"),
                    data.frame("victories"=s1943, "year" = "1943"),
                    data.frame("victories"=s1944, "year" = "1944"))

path2 <- "/Users/apple/UH-CPL/ACEs/data_02052020"

BIO_raw <- read.csv(paste("/Users/apple/Downloads/Data-5/Scholars/NIH_BIO_scholars_QC_temporal_v2.csv", sep=''),
                    header = T)
CS_raw <- read.csv(paste("/Users/apple/Downloads/Data-5/Scholars/NSF_CS_scholars_QC_temporal_v2.csv", sep=''),
                   header = T)

#'started 1996 - 2000', 
#'started 2001 - 2005', 
#'started 2006 - 2010'

# Scholars generations
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

data_scholars_CS <- rbind(data.frame("victories"=CS_s1996, "year" = "1996"), 
                          data.frame("victories"=CS_s2001, "year" = "2001"),
                          data.frame("victories"=CS_s2006, "year" = "2006"))

data_scholars_BIO <- rbind(data.frame("victories"=BIO_s1996, "year" = "1996"), 
                           data.frame("victories"=BIO_s2001, "year" = "2001"),
                           data.frame("victories"=BIO_s2006, "year" = "2006"))

# Load Athletes US
data_path <- "/Users/apple/UH-CPL/ACEs/data_02052020/scripts_results/"
load(paste(paste(data_path, "ATHLETES_USA_SWIMMERS_4_GENS", sep=""),"/results2.RData",sep=""))
data_athletes_US <- rbind(data.frame("victories"=tmpSCL[[1]][["1896-1912"]], "year" = "1896-1912"),
                          data.frame("victories"=tmpSCL[[1]][["1920-1936"]], "year" = "1920-1936"),
                          data.frame("victories"=tmpSCL[[1]][["1948-1976"]], "year" = "1948-1976"),
                          data.frame("victories"=tmpSCL[[1]][["1980-2016"]], "year" = "1980-2016"))

# Load Athletes FRA
data_path <- "/Users/apple/UH-CPL/ACEs/data_02052020/scripts_results/"
load(paste(paste(data_path, "ATHLETES_FRA_FENCING_2_GR", sep=""),"/results2.RData",sep=""))
data_athletes_FR <- rbind(data.frame("victories"=tmpSCL[[2]][["1896-1912"]], "year" = "1896-1912"),
                          data.frame("victories"=tmpSCL[[2]][["1920-1936"]], "year" = "1920-1936"),
                          data.frame("victories"=tmpSCL[[2]][["1948-1976"]], "year" = "1948-1976"),
                          data.frame("victories"=tmpSCL[[2]][["1980-2016"]], "year" = "1980-2016"))

# TEST BATTERY

# Statistical hypotheses. For all these tests that follow, the null hypothesis is that all 
# populations variances are equal, the alternative hypothesis is that at least two of them differ. 
# Consequently, p-values less than 0.05 suggest variances are significantly different and 
# the homogeneity of variance assumption has been violated.
dataframes <- list(data_pilots, 
                   data_scholars_CS, data_scholars_BIO, 
                   data_athletes_US, data_athletes_FR)
datasets <- list("Pilots GER", 
                 "Scholars CS", "Scholars BIO", 
                 "Athletes US", "Athletes FR")
i <- 1

for (data_input in dataframes) {
    print("----------------------------------------------------------")
    tryCatch(
        {
            print(paste("Bartlett test for: ", datasets[i]))
            print(bartlett.test(victories ~ year, data = data_input))
            print(paste("Bartlett test for lognormal transformed: ", datasets[i]))
            print(bartlett.test(log(victories) ~ year, data = data_input))
        }, error=function(cond) {
            print("Error in Bartlett test")
        }
    )
    
    tryCatch(
        {
        print(paste("Levene test test for: ", datasets[i]))
        print(leveneTest(victories ~ year, data = data_input))
        print(paste("Levene test test for lognormal transformed: ", datasets[i]))
        print(leveneTest(log(victories) ~ year, data = data_input))
        }, error=function(cond) {
            print("Error in Levene test")
        }
    )
    
    tryCatch(
        {
    print(paste("Flinger test test for: ", datasets[i]))
    print(fligner.test(victories ~ year, data = data_input))
    print(paste("Flinger test test for lognormal transformed: ", datasets[i]))
    print(fligner.test(log(victories) ~ year, data = data_input))
    print("----------------------------------------------------------")
        }, error=function(cond) {
            print("Error in Flinger test")
        }
    )
    i <<- i + 1
}


data_pilots_strip <- list("1939" = s1939, 
                          "1940" = s1940,
                          "1941" = s1941,
                          "1942" = s1942,
                          "1943" = s1943,
                          "1944" = s1944)
    
stripchart(data_pilots_strip,
           main="Pilots GER, multiple stripchart",
           xlab="Victories",
           ylab="Time frames",
           method="jitter",
           col=c("orange","red"),
           pch=16
)

data_scholars_CS_strip <- list("1996" = CS_s1996,
                               "2001" = CS_s2001,
                               "2006" = CS_s2006)

data_scholars_BIO_strip <- list("1996" = BIO_s1996,
                               "2001" = BIO_s2001,
                               "2006" = BIO_s2006)

stripchart(data_scholars_CS_strip,
           main="Scholars CS, multiple stripchart",
           xlab="Victories",
           ylab="Time frames",
           method="jitter",
           col=c("blue","gold"),
           pch=16
)

stripchart(data_scholars_BIO_strip,
           main="Scholars BIO, multiple stripchart",
           xlab="Victories",
           ylab="Time frames",
           method="jitter",
           col=c("green","magenta"),
           pch=16
)

# Load Athletes US
data_path <- "/Users/apple/UH-CPL/ACEs/data_02052020/scripts_results/"
load(paste(paste(data_path, "ATHLETES_USA_SWIMMERS_4_GENS", sep=""),"/results2.RData",sep=""))
data_athletes_US_strip <- list("1896-" = tmpSCL[[1]][["1896-1912"]],
                               "1920-" = tmpSCL[[1]][["1920-1936"]],
                               "1948-" = tmpSCL[[1]][["1948-1976"]],
                               "1980-" = tmpSCL[[1]][["1980-2016"]])

stripchart(data_athletes_US_strip,
           main="Athletes US, multiple stripchart",
           xlab="Victories",
           ylab="Time frames",
           method="jitter",
           col=c("darkgrey","orange"),
           pch=16
)

# Load Athletes FRA
data_path <- "/Users/apple/UH-CPL/ACEs/data_02052020/scripts_results/"
load(paste(paste(data_path, "ATHLETES_FRA_FENCING_2_GR", sep=""),"/results2.RData",sep=""))
data_athletes_FRA_strip <- list("1896-" = tmpSCL[[2]][["1896-1912"]],
                               "1920-" = tmpSCL[[2]][["1920-1936"]],
                               "1948-" = tmpSCL[[2]][["1948-1976"]],
                               "1980-" = tmpSCL[[2]][["1980-2016"]])

stripchart(data_athletes_FRA_strip,
           main="Athletes FRA, multiple stripchart",
           xlab="Victories",
           ylab="Time frames",
           method="jitter",
           col=c("blue","black"),
           pch=16
)

# DEBUGGING LEVENE TEST FOR LOG US / FRA ATHLETES

summary(dataframes[[4]]$victories)
summary(log(dataframes[[4]]$victories))
summary(dataframes[[4]]$year)
x <- c(0,1,2,3)
x[x>0]
dataframes[[4]] <- subset (dataframes[[4]], victories >0)
dataframes[[5]] <- subset (dataframes[[5]], victories >0)
leveneTest(log(victories) ~ year, data = dataframes[[4]])
leveneTest(log(victories) ~ year, data = dataframes[[5]])
