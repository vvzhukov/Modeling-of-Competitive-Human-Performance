# TOTAL
library(dplyr)
library(data.table)
library(plotly)

## ATHLETES
path <- "/Users/apple/UH-CPL/ACEs/data_athletes/120-years-of-olympic-history-athletes-and-results" ## Path to data
path2 <- "/Users/apple/UH-CPL/ACEs/data_12122019" ## Path to save data
path2 <- "/Users/apple/UH-CPL/ACEs/data_02052020" ## Path to save data
data_athletes1 <- read.csv(paste(path,"/athlete_events.csv", sep=''),
                           header = T)

data_athletes1$gold = ifelse(data_athletes1$Medal == "Gold", 1, 0) 
data_athletes1$gold[is.na(data_athletes1$gold)] <- 0
data_athletes1$silver = ifelse(data_athletes1$Medal == "Silver", 1, 0)
data_athletes1$silver[is.na(data_athletes1$silver)] <- 0
data_athletes1$bronze = ifelse(data_athletes1$Medal == "Bronze", 1, 0)
data_athletes1$bronze[is.na(data_athletes1$bronze)] <- 0

data_athletes_US <- subset(data_athletes1, NOC=="USA" & Sport=="Swimming")
data_athletes_FRA <- subset(data_athletes1, NOC=="FRA" & Sport=="Fencing")

    
data_athletes_US %>% group_by(Games) %>% dplyr::summarise (total_records = n(), total_winners = sum(bronze+gold+silver)) -> a_summ_US
a_summ_US$Success_rate <- a_summ_US$total_winners/a_summ_US$total_records

data_athletes_FRA %>% group_by(Games) %>% dplyr::summarise (total_records = n(), total_winners = sum(bronze+gold+silver)) -> a_summ_FRA
a_summ_FRA$Success_rate <- a_summ_FRA$total_winners/a_summ_FRA$total_records




fig1 <- plot_ly(
    x = ~ factor(a_summ_US$Games),
    y = ~ a_summ_US$Success_rate,
    name = "SF Zoo",
    type = "bar"
)
fig1
summary(a_summ_US$Success_rate)


fig2 <- plot_ly(a_summ_FRA,
                x = ~ factor(Games),
                y = ~ Success_rate,
                marker = list(color = c('#CC1480')),
                name = "SF Zoo",
                type = "bar"
) 
fig2
summary(a_summ_FRA$Success_rate)

# Merged plot (TEAM)

merged_athletes <- merge(a_summ_US, a_summ_FRA, by.x = "Games", by.y = "Games")


plot_ly(data = merged_athletes, x = ~factor(Games), y = ~Success_rate.x, type = 'bar', name = 'USA swimming', 
        marker = list(color = 'rgb(0,0,0)')) %>%
    add_trace(y = ~Success_rate.y, name = 'FRA fencing', type = 'bar', width = 0.3, 
              marker = list(color = 'rgb(255,165,0)')) %>%
    layout(yaxis = list(title = 'Success rate (medals per participants)'), barmode = 'overlay') %>%
    layout(title = 'Success rate in athletes competitions (in TEAM)',
           xaxis = list(title = "", showgrid = TRUE),
           yaxis = list(title = "Success rate (medals per participants)", showgrid = TRUE))



# Merged plot (OVERALL)

data_athletes_SW <- subset(data_athletes1, Sport=="Swimming")
data_athletes_FE <- subset(data_athletes1, Sport=="Fencing")


data_athletes_SW %>% group_by(Games) %>% dplyr::summarise (total_records = n(), total_winners = sum(bronze+gold+silver)) -> a_summ_SW
a_summ_SW$Success_rate <- a_summ_SW$total_winners/a_summ_SW$total_records

data_athletes_FE %>% group_by(Games) %>% dplyr::summarise (total_records = n(), total_winners = sum(bronze+gold+silver)) -> a_summ_FE
a_summ_FE$Success_rate <- a_summ_FE$total_winners/a_summ_FE$total_records


merged_athletes_sport <- merge(a_summ_SW[-c(1,2,3),], a_summ_FE[-c(1,2,3),], by.x = "Games", by.y = "Games")


plot_ly(data = merged_athletes_sport, x = ~factor(Games), y = ~Success_rate.x, type = 'bar', name = 'Swimming', 
        marker = list(color = 'rgb(0,0,0)')) %>%
    add_trace(y = ~Success_rate.y, name = 'Fencing', type = 'bar', width = 0.3, 
              marker = list(color = 'rgb(255,165,0)')) %>%
    layout(yaxis = list(title = 'Success rate (medals per participants)'), barmode = 'overlay') %>%
    layout(title = 'Success rate in athletes competitions (in SPORT)',
           xaxis = list(title = "", showgrid = TRUE),
           yaxis = list(title = "Success rate (medals per participants)", showgrid = TRUE))


# Merged plot (OVERALL, epochs)

data_athletes_SW <- subset(data_athletes1, Sport=="Swimming")
data_athletes_FE <- subset(data_athletes1, Sport=="Fencing")


data_athletes_SW %>% group_by(Games) %>% dplyr::summarise (total_records = n(), total_winners = sum(bronze+gold+silver)) -> a_summ_SW
a_summ_SW$Success_rate <- a_summ_SW$total_winners/a_summ_SW$total_records

data_athletes_FE %>% group_by(Games) %>% dplyr::summarise (total_records = n(), total_winners = sum(bronze+gold+silver)) -> a_summ_FE
a_summ_FE$Success_rate <- a_summ_FE$total_winners/a_summ_FE$total_records


merged_athletes_sport <- merge(a_summ_SW[-c(1,2,3),], a_summ_FE[-c(1,2,3),], by.x = "Games", by.y = "Games")

a_epochs <- c("1906 - 1912", "1920 - 1936", "1948 - 1976", "1980 - 2016")

a_sw <- c(mean(merged_athletes_sport$Success_rate.x[1:3]),
          mean(merged_athletes_sport$Success_rate.x[4:8]),
          mean(merged_athletes_sport$Success_rate.x[9:16]),
          mean(merged_athletes_sport$Success_rate.x[17:26]))

a_fe <- c(mean(merged_athletes_sport$Success_rate.y[1:3]),
          mean(merged_athletes_sport$Success_rate.y[4:8]),
          mean(merged_athletes_sport$Success_rate.y[9:16]),
          mean(merged_athletes_sport$Success_rate.y[17:26]))

merged_athletes_sport_epochs <- data.frame(a_epochs, a_sw, a_fe)

plot_ly(data = merged_athletes_sport_epochs, x = ~factor(a_epochs), y = ~a_sw, type = 'bar', name = 'Swimming', 
        marker = list(color = 'rgb(0,0,0)')) %>%
    add_trace(y = ~a_fe, name = 'Fencing', type = 'bar', width = 0.3, 
              marker = list(color = 'rgb(255,165,0)')) %>%
    layout(yaxis = list(title = 'Success rate (medals per participants)'), barmode = 'overlay') %>%
    layout(title = 'Success rate in athletes competitions (in SPORT, epochs)',
           xaxis = list(title = "", showgrid = TRUE),
           yaxis = list(title = "Success rate (medals per participants)", showgrid = TRUE))



# SCHOLARS

sc_years <- c(2001:2013)
sc_nsf <- c(34, 37, 36, 29, 28, 24, 23, 21, 33, 23, 21, 15, 16)
sc_nih <- c(32, 31, 30 , 25, 23, 21, 24, 23, 22, 22, 19, 18, 17)

sc_data <- data.frame(sc_years,
                      sc_nsf,
                      sc_nih)

plot_ly(data = sc_data, x = ~factor(sc_years), y = ~sc_nsf, name = 'NSF', type = 'bar') %>%
    add_trace(y = ~sc_nih, name = 'NIH', type = 'bar', width = 0.4) %>%
    layout(title = '\n NSF/NIH Success Rate by Fiscal Year',
           barmode = 'overlay',
           xaxis = list(title = "", showgrid = TRUE),
           yaxis = list(title = "Percent Success Rate", showgrid = TRUE))

# SCHOLARS groups

sc_years <- c("2001-2003","2004-2008","2009-2013")
sc_nsf <- c(mean(c(34, 37, 36)), mean(c(29, 28, 24, 23, 21)), mean(c(33, 22, 21, 15, 16)))
sc_nih <- c(mean(c(32, 31, 30)) , mean(c(25, 23, 21, 24, 23)), mean(c(22, 22, 19, 18, 17)))

sc_data <- data.frame(sc_years,
                      sc_nsf,
                      sc_nih)

plot_ly(data = sc_data, x = ~factor(sc_years), y = ~sc_nsf, name = 'NSF', type = 'bar') %>%
    add_trace(y = ~sc_nih, name = 'NIH', type = 'bar', width = 0.3) %>%
    layout(title = 'Success rate in scholars competitions (epochs)',
           barmode = 'overlay',
           xaxis = list(title = "", showgrid = TRUE),
           yaxis = list(title = "Success rate (grants per participants)", showgrid = TRUE))


# PILOTS

pr_years <- c("1940", "1941", "1942", "1943", "1944", "1945")

pr_rates <- c(0.154, 0.186, 0.146, 0.219, 0.248, 0.185)


pr_data <- data.frame(pr_years,
                      pr_rates)

plot_ly(data = pr_data, x = ~factor(pr_years), y = pr_rates, name = 'NSF', type = 'bar', 
        marker = list(color = 'rgb(128,0,0)'))  %>%
    layout(title = 'Survival rate in pilots competitions (years)',
           barmode = 'overlay',
           xaxis = list(title = "", showgrid = TRUE),
           yaxis = list(title = "Success rate (% of survived SE units)", showgrid = TRUE))
