library(car)

### Folder where we have the data
path <- "/Users/apple/UH-CPL/ACEs/data_02052020"
pilots_raw <- read.csv(paste(path,"/Pilots_breakdown_GER.csv", sep=''))

pilots_raw$
# Pilots generations
s1939 <- rowSums(subset(pilots_raw[,3:8], y1939 != 0))
s1940 <- rowSums(subset(pilots_raw[,3:8], y1940 != 0 & y1939 == 0)[,2:6])
s1941 <- rowSums(subset(pilots_raw[,3:8], y1941 != 0 & y1940 == 0 & y1939 == 0)[,3:6])
s1942 <- rowSums(subset(pilots_raw[,3:8], y1942 != 0 & y1941 == 0 & y1940 == 0 & y1939 == 0)[,4:6])
s1943 <- rowSums(subset(pilots_raw[,3:8], y1943 != 0 & y1942 == 0 & y1941 == 0 & y1940 == 0 & y1939 == 0)[,5:6])
s1944 <- subset(pilots_raw[,3:9], y1944 != 0 & y1943 == 0 & y1942 == 0 & y1941 == 0 & y1940 == 0 & y1939 == 0)[,6]

sYears <- list(s1939, s1940, s1941, s1942, s1943, s1944)

data_pilots <- rbind(data.frame("victories"=s1939, "year" = "1939"), 
                    data.frame("victories"=s1940, "year" = "1940"),
                    data.frame("victories"=s1941, "year" = "1941"),
                    data.frame("victories"=s1942, "year" = "1942"),
                    data.frame("victories"=s1943, "year" = "1943"),
                    data.frame("victories"=s1944, "year" = "1944"))

data_pilots_n5 <- rbind(data.frame("victories"=s1939, "year" = "1939"), 
                     data.frame("victories"=s1940, "year" = "1940"),
                     data.frame("victories"=s1941, "year" = "1941"),
                     data.frame("victories"=s1942, "year" = "1942"),
                     data.frame("victories"=s1944, "year" = "1944"))

data_pilots_n4 <- rbind(data.frame("victories"=s1939, "year" = "1939"), 
                        data.frame("victories"=s1940, "year" = "1940"),
                        data.frame("victories"=s1941, "year" = "1941"),
                        data.frame("victories"=s1942, "year" = "1942"))


data_pilots_n3 <- rbind(data.frame("victories"=s1939, "year" = "1939"), 
                         data.frame("victories"=s1940, "year" = "1940"),
                         data.frame("victories"=s1941, "year" = "1941"))

dataframes <- list(data_pilots,
                   data_pilots_n5,
                   data_pilots_n4,
                   data_pilots_n3
                   )
datasets <- list("Pilots GER",
                 "Pilots GER no 1943",
                 "Pilots GER first 4 years",
                 "Pilots GER first 3 years")

i <- 1
for (data_input in dataframes) {
    print("----------------------------------------------------------")
    tryCatch(
        {
        print(paste("Levene test test for lognormal transformed: ", datasets[i]))
        print(leveneTest(log(victories) ~ year, data = data_input))
        }, error=function(cond) {
            print("Error in Levene test")
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

par(mfrow=c(2,3))
plot(density(s1939), main = paste("Generation 1939. Variance: ", round(var(s1939),2)))
plot(density(s1940), main = paste("Generation 1940. Variance: ", round(var(s1940),2)))
plot(density(s1941), main = paste("Generation 1941. Variance: ", round(var(s1941),2)))
plot(density(s1942), main = paste("Generation 1942. Variance: ", round(var(s1942),2)))
plot(density(s1943), main = paste("Generation 1943. Variance: ", round(var(s1943),2)))
plot(density(s1944), main = paste("Generation 1944. Variance: ", round(var(s1944),2)))


for (i in 2:length(sYears)) {
    for (j in 1:(i-1)) {
        result <- leveneTest(log(victories) ~ year, data = rbind(data.frame("victories"=unlist(sYears[i]), "year" = "1"),
                                                       data.frame("victories"=unlist(sYears[j]), "year" = "2")))$`Pr(>F)`[1]
        print("----------------------")
        print(paste("Comparing years: ", 1938 + i, "vs", 1938 + j))
        if (result < 0.05) {
            #print(paste("Truncating ", 1938 + j))
            temp1 <- sort(unlist(sYears[i]))
            temp2 <- sort(unlist(sYears[j]))
            iterations <- 0
            
            while (result < 0.05) {
                # exclude top value
                temp2 <- temp2[-length(temp2)]
                
                result <- leveneTest(log(victories) ~ year, data = rbind(data.frame("victories"=temp1, "year" = "1"),
                                                                         data.frame("victories"=temp2, "year" = "2")))$`Pr(>F)`[1]
                iterations <- iterations + 1
            }
            data_trimmed = iterations * 100 / length(unlist(sYears[j]))
            print(paste("Test passed on", iterations, "top records, [", round(data_trimmed,2),"% ] of",1938 + j, "records trimmed."))
            
            result <- 0
            temp1 <- sort(unlist(sYears[i]))
            temp2 <- sort(unlist(sYears[j]))
            iterations <- 0
            
            while (result < 0.05) {
                # exclude by factor level, top values
                temp2 <- temp2[temp2 != max(temp2)]
                
                result <- leveneTest(log(victories) ~ year, data = rbind(data.frame("victories"=temp1, "year" = "1"),
                                                                         data.frame("victories"=temp2, "year" = "2")))$`Pr(>F)`[1]
                iterations <- iterations + 1
            }
            data_trimmed = 100 - length(temp2) * 100 / length(unlist(sYears[j]))
            print(paste("Test passed on", iterations, "top levels, [", round(data_trimmed,2),"% ] of",1938 + j, "records trimmed."))
            
            result <- 0
            temp1 <- sort(unlist(sYears[i]))
            temp2 <- sort(unlist(sYears[j]))
            iterations <- 0
            
            while (result < 0.05) {
                
                # exclude by factor level, bottom values
                temp2 <- temp2[temp2 != min(temp2)]
                
                result <- leveneTest(log(victories) ~ year, data = rbind(data.frame("victories"=temp1, "year" = "1"),
                                                                         data.frame("victories"=temp2, "year" = "2")))$`Pr(>F)`[1]
                iterations <- iterations + 1
            }
            data_trimmed = 100 - length(temp2) * 100 / length(unlist(sYears[j]))
            print(paste("Test passed on", iterations, "bot levels, [", round(data_trimmed,2),"% ] of",1938 + j, "records trimmed."))
            
            # ------ same for the second value (temp1 / j)
            result <- 0
            temp1 <- unname(sort(unlist(sYears[i])))
            temp2 <- unname(sort(unlist(sYears[j])))
            iterations <- 0
            
            while (result < 0.05) {
                # exclude top value
                temp1 <- temp1[-length(temp1)]
                
                result <- leveneTest(log(victories) ~ year, data = rbind(data.frame("victories"=temp2, "year" = "1"),
                                                                         data.frame("victories"=temp1, "year" = "2")))$`Pr(>F)`[1]
                iterations <- iterations + 1
            }
            data_trimmed = iterations * 100 / length(unlist(sYears[i]))
            print(paste("Test passed on", iterations, "top records, [", round(data_trimmed,2),"% ] of",1938 + i, "records trimmed."))
            
            result <- 0
            temp1 <- sort(unlist(sYears[i]))
            temp2 <- sort(unlist(sYears[j]))
            iterations <- 0
            
            while (result < 0.05) {
                # exclude by factor level, top values
                temp1 <- temp1[temp1 != max(temp1)]
                if (length(temp1) == 0) {
                    break
                }
                result <- leveneTest(log(victories) ~ year, data = rbind(data.frame("victories"=temp2, "year" = "1"),
                                                                         data.frame("victories"=temp1, "year" = "2")))$`Pr(>F)`[1]
                iterations <- iterations + 1
            }
            data_trimmed = 100 - length(temp1) * 100 / length(unlist(sYears[i]))
            print(paste("Test passed on", iterations, "top levels, [", round(data_trimmed,2),"% ] of",1938 + i, "records trimmed."))
            
            result <- 0
            temp1 <- sort(unlist(sYears[i]))
            temp2 <- sort(unlist(sYears[j]))
            iterations <- 0
            
            while (result < 0.05) {
                
                # exclude by factor level, bottom values
                temp1 <- temp1[temp1 != min(temp1)]
                
                result <- leveneTest(log(victories) ~ year, data = rbind(data.frame("victories"=temp2, "year" = "1"),
                                                                         data.frame("victories"=temp1, "year" = "2")))$`Pr(>F)`[1]
                iterations <- iterations + 1
            }
            data_trimmed = 100 - length(temp1) * 100 / length(unlist(sYears[i]))
            print(paste("Test passed on", iterations, "bot levels, [", round(data_trimmed,2),"% ] of",1938 + i, "records trimmed."))
            
        } else {
            print("Tested passed without any data trimmed.")
        }
    }
}

