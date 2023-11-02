plot(sort(BIO_I$y2016), xlab ='NIH BIO faculties', ylab ='Grant victories', xaxt='n')
abline(v = 220, col="red", lwd=3, lty=2)

boxplot(log(sort(BIO_I$y2016)))



dev.off()
plot(cars)
abline(v=15, col="blue")


install.packages('ggstatsplot')
install.packages('data.table')
library(ggstatsplot)

gen1 <- data.frame(BIO_s1996)
colnames(gen1) <- "Score"
gen1$gen <- 1996

gen2 <- data.frame(BIO_s2001)
colnames(gen2) <- "Score"
gen2$gen <- 2001

gen3 <- data.frame(BIO_s2006)
colnames(gen3) <- "Score"
gen3$gen <- 2006

test <- rbind(gen1, gen2, gen3)

set.seed(123)
library(ggstatsplot)

# simple function call with the defaults
ggstatsplot::ggbetweenstats(
    data = nih1g_top_10,
    x = gen_str,
    y = cost_k,
    title = "BIO NIH grant ictories across generations",
    caption = "Generation separated by the 4 years interval, ex. 1996 - 2000"
)


library("readxl")
nih1g_top_10 <- read_excel("/Users/apple/Downloads/NIH1g_TOP_10.xlsx")
nih1g_top_10[,c(2,3)] <- NULL

nih1g_top_10$gen_str <- ''

nih1g_top_10[nih1g_top_10$FY >= 1996 & nih1g_top_10$FY <=2000,]$gen_str <- '1996-2000' 
nih1g_top_10[nih1g_top_10$FY >= 2001 & nih1g_top_10$FY <=2005,]$gen_str <- '2001-2005'
nih1g_top_10[nih1g_top_10$FY >= 2006 & nih1g_top_10$FY <=2010,]$gen_str <- '2006-2011'
nih1g_top_10[nih1g_top_10$FY >= 2011 & nih1g_top_10$FY <=2016,]$gen_str <- '2012-2016'

#1996-2000, 2001-2005, 2006-2010, 2011-2016
nih1g_top_10$cost_k <-  round(nih1g_top_10$Cost/1000)


library("yarrr")


pirateplot(formula = cost_k ~ gen_str,
           data = nih1g_top_10,
           main = "NIH grant funding by stages (top 20 competitors), 1st generation \n n = 1149",
           theme = 1,
           xlab="Stages (5-years period, from 1996 to 2016)",
           ylab="Grant size (thousands us dollars)")

pirateplot(formula = cost_k ~ FY,
           data = nih1g_top_10[nih1g_top_10$FY %in% c(1996,1997,1998,1999,2000),],
           main = "NIH grant funding by years (top 20 competitors), 1st generation \n n = 280",
           theme = 2,
           xlab="1 stage",
           ylab="Grant size (thousands us dollars)")



# Repair funding data for 1996 - 1999

#filse 1996-1999, required fund correction
files_r <- list.files(path = "/Users/apple/UH-CPL/ACEs/data_scholars/fundings_data_NIH/to_update_fundings", pattern = "*.csv", full.names = T)

#files with the corrections
files_f <- list.files(path = "/Users/apple/UH-CPL/ACEs/data_scholars/fundings_data_NIH/to_update_fundings/corrections", pattern = "*.csv", full.names = T) 

result_r <- data.frame(APPLICATION_ID = integer(), FY = integer(), ORG_NAME = character(), ORG_STATE = character(), PI_NAMEs = character(), TOTAL_COST = integer())

for (file in files_r) {
    temp <- fread(file, select = c("APPLICATION_ID", "FY", "ORG_NAME", "ORG_STATE", "PI_NAMEs", "TOTAL_COST"))
    result_r <<- rbind(result_r, temp)
}

result_f <- data.frame(APPLICATION_ID = integer(), TOTAL_COST = integer())

for (file in files_f) {
    temp <- fread(file, select = c("APPLICATION_ID", "TOTAL_COST"))
    result_f <<- rbind(result_f, temp)
}

results_2 <- merge(result_r, result_f, by.x="APPLICATION_ID", by.y="APPLICATION_ID")
results_2[,6] <- NULL
colnames(results_2)[6] <- "TOTAL_COST"
results_2[,1] <- NULL

library(readr)
library(plyr)

files <- list.files(path = "/Users/apple/UH-CPL/ACEs/data_scholars/fundings_data_NIH", pattern = "*.csv", full.names = T)

# We need following columns: FY, ORG_NAME, ORG_STATE, PI_NAMEs, TOTAL_COST

library(data.table)

result <- data.frame(FY = integer(), ORG_NAME = character(), ORG_STATE = character(), PI_NAMEs = character(), TOTAL_COST = integer())

for (file in files) {
    temp <- fread(file, select = c("FY", "ORG_NAME", "ORG_STATE", "PI_NAMEs", "TOTAL_COST"))
    result <<- rbind(result, temp)
}

result <- rbind(result, results_2)
remove(result_f, result_r, results_2, temp, file, files, files_r, files_f)

write.csv(result, '/Users/apple/UH-CPL/ACEs/data_scholars/fundings_data_NIH/all_data.csv')


# Plotting part

dev.off()

require(ggplot2)
ggplot(data = result, aes(x=FY, y=TOTAL_COST)) + geom_boxplot(aes(group=FY))


pirateplot(formula = TOTAL_COST/1000 ~ FY,
           data = result[result$FY %in% c(1997,1998,1999, 2000, 2001),],
           main = "NIH grant funding by years",
           theme = 2,
           point.cex = 0,
           cap.beans = T,
           back.col = transparent("blue", .95), # Add light blue background
           gl.col = "gray", # Gray gridlines
           gl.lwd = c(.75, 0),
           inf.f.o = .6, # Turn up inf filling
           inf.disp = "bean", # Wrap inference around bean
           bean.b.o = .4, # Turn down bean borders
           quant = c(.1, .9), # 10th and 90th quantiles
           quant.col = "black", # Black quantile lines
           xlab="Fiscal year",
           ylab="Grant size (thousands us dollars)",
           ylim = c(0,700))


boxplot(result$TOTAL_COST/1200, ylim = c(0,1200))

summary(result$TOTAL_COST[result$TOTAL_COST>0])