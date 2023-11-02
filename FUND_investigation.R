# UPLOAD data
NSF_fund <- read.csv("/Users/apple/UH-CPL/ACEs/data_scholars/fundings_data_NSF/Scholars_NSF_fund_data.csv")
NIH_fund <- read.csv("/Users/apple/UH-CPL/ACEs/data_scholars/fundings_data_NIH/Scholars_NIH_fund_data.csv")

# Inflation rates from 1996 - 2016 inc (https://www.usinflationcalculator.com/inflation/historical-inflation-rates/)
Inflation_rates <- c(0,
                     2.3,
                    1.6,
                    2.2,
                    3.4,
                    2.8,
                    1.6,
                    2.3,
                    2.7,
                    3.4,
                    3.2,
                    2.8,
                    3.8,
                    -0.4,
                    1.6,
                    3.2,
                    2.1,
                    1.5,
                    1.6,
                    0.1,
                    1.3)

Inflation_rates_cumulative <- c()
base <- 1
for (i in Inflation_rates) {
    base <<- base * (1 - i/100)
    Inflation_rates_cumulative <<- c(Inflation_rates_cumulative,base)
}

# Calculating inflation impact as 
NIH_fund$INF_TOTAL_COST <- NIH_fund$TOTAL_COST * Inflation_rates_cumulative[NIH_fund$FY-1996]
NSF_fund$INF_TOTAL_COST <- NSF_fund$TOTAL_COST * Inflation_rates_cumulative[NSF_fund$FY-1996]

NIH_fund <- NIH_fund[NIH_fund$TOTAL_COST>=0,]
NSF_fund <- NSF_fund[NSF_fund$TOTAL_COST>=0,]

# Visualisations

require(ggplot2)
require("yarrr")

pirateplot(formula = INF_TOTAL_COST/1000 ~ FY,
           data = NSF_fund,
           main = paste("NSF grant funding 1996 - 2016, \n n = ", nrow(NSF_fund)),
           #theme = 2,
           point.cex = 0.1,
           #cap.beans = T,
           back.col = transparent("blue", .95), # Add light blue background
           gl.col = "gray", # Gray gridlines
           gl.lwd = c(.75, 0),
           #inf.f.o = .6, # Turn up inf filling
           #inf.disp = "bean", # Wrap inference around bean
           #bean.b.o = .4, # Turn down bean borders
           quant = c(.1, .9), # 10th and 90th quantiles
           quant.col = "darkblue", # Black quantile lines
           xlab="Fiscal year",
           ylab="Grant size (thousands us dollars), including inflation",
           ylim = c(0,1000),
           bean.f.o = 0,
           bean.lty = 0)

pirateplot(formula = INF_TOTAL_COST/1000 ~ FY,
           data = NIH_fund,
           main = paste("NIH grant funding 1996 - 2016, \n n = ", nrow(NIH_fund)),
           #theme = 2,
           point.cex = 0.1,
           #cap.beans = T,
           back.col = transparent("blue", .95), # Add light blue background
           gl.col = "gray", # Gray gridlines
           gl.lwd = c(.75, 0),
           #inf.f.o = .6, # Turn up inf filling
           #inf.disp = "bean", # Wrap inference around bean
           #bean.b.o = .4, # Turn down bean borders
           quant = c(.1, .9), # 10th and 90th quantiles
           quant.col = "darkblue", # Black quantile lines
           xlab="Fiscal year",
           ylab="Grant size (thousands us dollars), including inflation",
           ylim = c(200,500),
           bean.f.o = 0,
           bean.lty = 0)

require(pastecs)
stat.desc(NIH_fund$INF_TOTAL_COST/1000)
summary(NIH_fund$INF_TOTAL_COST/1000)
summary(NSF_fund$INF_TOTAL_COST/1000)

sum(NIH_fund$INF_TOTAL_COST/1000, na.rm = T)
sum(NSF_fund$INF_TOTAL_COST/1000, na.rm = T)