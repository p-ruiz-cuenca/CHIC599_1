# Load data and packages ====

library(ggplot2)
library(lubridate)

covid <- read.csv("data/covid_data.csv")

covid$date <- ymd(covid$date)

# Create "t" variable 
covid$t <- as.numeric(covid$date)-(min(as.numeric(covid$date)))+1

## Create binary lockdown variable ----
covid$lockdown <- rep(0, length(covid$date))

# second lockdown
covid$lockdown[covid$date > ymd("2020-11-04") & 
                 covid$date < ymd("2020-12-02")] <- 1

# third lockdown 
covid$lockdown[covid$date > ymd("2020-12-25") & 
                 covid$date < ymd("2021-03-30")] <- 1

# Plot cases and lockdown 

ggplot(covid, aes(x = t))+
  geom_line(aes(y = newCasesBySpecimenDate))+
  annotate("rect", xmin = 132, xmax = 158,
           ymin = -500, ymax = max(covid$newCasesBySpecimenDate)+500,
           fill = "red", alpha = 0.1)+
  annotate("rect", xmin = 183, xmax = 276,
           ymin = -500, ymax = max(covid$newCasesBySpecimenDate)+500,
           fill = "red", alpha = 0.1)+
  scale_y_continuous("New Cases", expand = c(0,0))+
  xlab("Time (days)")+
  theme_light()

ggplot(covid, aes(x = t))+
  geom_line(aes(y = log(newCasesBySpecimenDate)))+
  annotate("rect", xmin = 132, xmax = 158,
           ymin = -0.2, ymax = max(log(covid$newCasesBySpecimenDate))+0.2,
           fill = "red", alpha = 0.1)+
  annotate("rect", xmin = 183, xmax = 276,
           ymin = -0.2, ymax = max(log(covid$newCasesBySpecimenDate))+0.2,
           fill = "red", alpha = 0.1)+
  scale_y_continuous("log(New Cases)", expand = c(0,0))+
  xlab("Time (days)")+
  theme_light()
  

# Create lagged variables ====

create.lag.var <- function(x,lag,data) {
  tlag <- data$t-lag
  ind.lag <- sapply(tlag,function(k) {
    out <- which((data$t-k)==0)
    if(length(out)==0) out <- NA
    return(out)
  })
  x[ind.lag]
}


env.vars <- c("rain", "mean.temp", "min.temp", "max.temp")

for (i in 1:length(env.vars)) {
  
  lags <- c(2, 7, 10, 28)
  
  for (j in 1:length(lags)) {
    
    covid[[paste0(env.vars[i], ".lag.", lags[j])]] <- create.lag.var(x = covid[[env.vars[i]]],
                                                                     lag = lags[j],
                                                                     data = covid)
    
  }
  
}


any(covid$newCasesBySpecimenDate==0)

# create splines with bs() base spline from package "spline"
bs()
# knots -> give as a vector all the point you want the slope to change 


# Quick lm() playaround ====

lm.fit1 <- lm(log(newCasesBySpecimenDate) ~ t + sin(2*pi*t/7) + cos(2*pi*t/7)+
                lockdown,
              data = covid)
summary(lm.fit1)

covid$pred.cases <- predict(lm.fit1)

ggplot(covid, aes(x = t))+
  geom_line(aes(y = log(newCasesBySpecimenDate)), col = "black")+
  geom_line(aes(y = pred.cases), col = "red")
