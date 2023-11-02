# Investigation of the key factors for bad LN fitting in pilots dataset (1943)
# opened fronts
# replishement of human resources
# justification for the balance
# Data from http://dracobooks.com/The%20Defeat%20of%20the%20German%20Air%20Force.html

# training hour "German Air Forces"

library(plotly)

tr_years <- c("1940_Q1", "1940_Q2", "1940_Q3", "1940_Q4",
              "1941_Q1", "1941_Q2", "1941_Q3", "1941_Q4",
              "1942_Q1", "1942_Q2", "1942_Q3", "1942_Q4",
              "1943_Q1", "1943_Q2", "1943_Q3", "1943_Q4",
              "1944_Q1", "1944_Q2", "1944_Q3", "1944_Q4",
              "1945_Q1", "1945_Q2"
              )

tr_hours <- c(240, 240, 240, 240,
              240, 240, 240, 240,
              240, 240, 240, 205,
              205, 205, 170, 170,
              170, 170, 115, 115,
              115, 115
              )

tr_data <- data.frame(name = tr_years, hours = tr_hours)


# Basic Barplot
plot_ly(tr_data, x = ~name, y = ~hours, type = 'bar',
        marker = list(color = c(rep(rgb(0.3,0.3,0.3,0.2),12),rep(rgb(0.3,0.1,0.4,0.6),4),  rep(rgb(0.3,0.3,0.3,0.2),6)))) %>% 
    layout(title = "German air forces training hours (hours on average)",
                      xaxis = list(title = ""),
                      yaxis = list(title = "Training hours")) %>%
    layout(shapes=list(list(type='line', x0= 11.5, x1= 11.5, y0=0, y1=250, line=list(dash='dot', width=3)),
                       list(type='line', x0= 15.5, x1= 15.5, y0=0, y1=250, line=list(dash='dot', width=3))))



## FIGURE 2

fr_years <- c("1939_Q3", "1939_Q4",
              "1940_Q1", "1940_Q2", "1940_Q3", "1940_Q4",
              "1941_Q1", "1941_Q2", "1941_Q3", "1941_Q4",
              "1942_Q1", "1942_Q2", "1942_Q3", "1942_Q4",
              "1943_Q1", "1943_Q2", "1943_Q3", "1943_Q4",
              "1944_Q1", "1944_Q2", "1944_Q3", "1944_Q4",
              "1945_Q1")

fr_held_austria_germany <- c(500, 1090, 
                             1020, 1200, 1090, 920,
                             810, 400, 410, 510,
                             500, 520, 200, 220,
                             250, 190, 150, 170,
                             160, 160, 50, 40,
                             20)

fr_west_front_strategic_def <- c(0,0,
                                 0,0,0,0,
                                 0,0,0,0,
                                 0,0,400,400,
                                 400,470, 680, 790,
                                 780, 830, 380, 570,
                                 400)

fr_west_front_tactical_force <- c(0,0,
                                  0,0,0,0,
                                  0,0,0,0,
                                  0,0,0,0,
                                  0,0,0,0,
                                  500, 500, 920, 1200,
                                  510)

fr_mediterranean_tactical_force <- c(0,0,
                                     0,0,0,0,
                                     0,0,0,120,
                                     220, 170, 250, 230,
                                     300, 430, 250, 290,
                                     220, 200, 0, 0,
                                     0)

fr_russian_front_tactical_force <- c(0,0,
                                     0,0,0,0,
                                     0,870,680,530,
                                     500, 620, 620, 480,
                                     530, 500, 470, 370,
                                     390, 360, 400, 520,
                                     1200)

fr_data <- data.frame(fr_years, 
                      fr_held_austria_germany,
                      fr_west_front_strategic_def,
                      fr_west_front_tactical_force,
                      fr_mediterranean_tactical_force,
                      fr_russian_front_tactical_force)


plot_ly(data = fr_data, x = ~fr_years, y = ~fr_held_austria_germany, type = 'bar', name = 'Held in Germany/Austria') %>%
    add_trace(y = ~fr_west_front_strategic_def, name = 'Western front, defence') %>%
    add_trace(y = ~fr_mediterranean_tactical_force, name = 'Mediterraranean front, tactical') %>%
    add_trace(y = ~fr_west_front_tactical_force, name = 'Western front, tactical') %>%
    add_trace(y = ~fr_russian_front_tactical_force, name = 'Russian front, tactical') %>%
    layout(yaxis = list(title = 'Count'), barmode = 'stack') %>%
    layout(shapes=list(list(type='line', x0= 13.5, x1= 13.5, y0=0, y1=2500, line=list(dash='dot', width=3)),
                        list(type='line', x0= 17.5, x1= 17.5, y0=0, y1=2500, line=list(dash='dot', width=3))),
           title = 'Strength of single engine fighters in combat units',
           xaxis = list(title = "", showgrid = TRUE),
           yaxis = list(title = "Figther units", showgrid = TRUE))

## FIGURE 3

ls_years <- c("1939_Q3", "1939_Q4",
              "1940_Q1", "1940_Q2", "1940_Q3", "1940_Q4",
              "1941_Q1", "1941_Q2", "1941_Q3", "1941_Q4",
              "1942_Q1", "1942_Q2", "1942_Q3", "1942_Q4",
              "1943_Q1", "1943_Q2", "1943_Q3", "1943_Q4",
              "1944_Q1", "1944_Q2", "1944_Q3", "1944_Q4",
              "1945_Q1")

ls_total <- c(0,2,
              2,6,7,4,
              3,7,11,5,
              6,10,13,8,
              13,22,28,19,
              31,41,49,40,
              0)
ls_west_ger_front <- c(0,0,
                       0,0,0,0,
                       0,0,0,0,
                       0,0,0,0,
                       0,0,0,0,
                       21,31,40,0,
                       0)
ls_east_front <- c(0,0,
                   0,0,0,0,
                   0,0,8,3,
                   3,6,6,3,
                   4,6,7,4,
                   4,5,7,5,
                   0)

ls_data <- data.frame(ls_years,
                      ls_total,
                      ls_west_ger_front,
                      ls_east_front)

plot_ly(data = ls_data, x = ~ls_years, y = ~ls_total, type = 'bar', name = 'Total') %>%
    add_trace(y = ~ls_west_ger_front, name = 'Western front and Germany', type = 'bar') %>%
    add_trace(y = ~ls_east_front, name = 'Russian front', type = 'bar') %>%
    layout(yaxis = list(title = '% of total s/e fighters'), barmode = 'stack') %>%
    layout(shapes=list(list(type='line', x0= 13.5, x1= 13.5, y0=0, y1=50, line=list(dash='dot', width=3)),
                       list(type='line', x0= 17.5, x1= 17.5, y0=0, y1=50, line=list(dash='dot', width=3))),
           title = 'German single engine fighters destroyed and damaged',
           xaxis = list(title = "", showgrid = TRUE),
           yaxis = list(title = "Percent from all S/E units", showgrid = TRUE))  %>%
    layout(barmode = 'overlay')

# 3D combination of all 3 plots

merged_data <- merge(ls_data, fr_data, by.x="ls_years", by.y="fr_years")
merged_data <- merge(merged_data[-c(1,2),], tr_data, by.x="ls_years", by.y="name")


fig <- plot_ly(merged_data, x = ~ls_total, y = ~(fr_held_austria_germany + 
                                                     fr_mediterranean_tactical_force +
                                                     fr_russian_front_tactical_force +
                                                     fr_west_front_strategic_def +
                                                     fr_west_front_tactical_force), z = ~ls_years,
               marker = list(color = ~hours, colorscale = c('#FFE1A1', '#683531'), showscale = TRUE))
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'Losses (%)'),
                                   yaxis = list(title = 'Units at combat'),
                                   zaxis = list(title = '')),
                      annotations = list(
                          x = 1.13,
                          y = 1.05,
                          text = 'Miles/(US) gallon',
                          xref = 'paper',
                          yref = 'paper',
                          showarrow = FALSE
                      ))
fig


# Losses vs Units in combat

plot_ly(data = merged_data, x = ~ls_years, y = ~ls_total, type = 'bar', name = 'Losses') 
plot_ly(data = merged_data, x = ~ls_years, y = ~(fr_held_austria_germany + 
                                                     fr_mediterranean_tactical_force +
                                                     fr_russian_front_tactical_force +
                                                     fr_west_front_strategic_def +
                                                     fr_west_front_tactical_force)*(100 - ls_total)/100,
        marker = list(color = c('#CC1280')),
        type = 'bar', name = 'Units in combat') 



%>%
    add_trace(y = ~(fr_held_austria_germany + 
                        fr_mediterranean_tactical_force +
                        fr_russian_front_tactical_force +
                        fr_west_front_strategic_def +
                        fr_west_front_tactical_force), name = 'Units in combat', type = 'bar') 
    layout(yaxis = list(title = '% of total s/e fighters'), barmode = 'stack') %>%
    layout(shapes=list(list(type='line', x0= 13.5, x1= 13.5, y0=0, y1=50, line=list(dash='dot', width=3)),
                       list(type='line', x0= 17.5, x1= 17.5, y0=0, y1=50, line=list(dash='dot', width=3))),
           title = 'German single engine fighters destroyed and damaged',
           xaxis = list(title = "", showgrid = TRUE),
           yaxis = list(title = "Percent from all S/E units", showgrid = TRUE))  %>%
    layout(barmode = 'overlay')
