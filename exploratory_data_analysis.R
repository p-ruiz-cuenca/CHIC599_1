# Load data and packages ====

library(ggplot2)
library(lubridate)

covid <- read.csv("data/covid_data.csv")

covid$date <- ymd(covid$date)

library(expss)

covid <- apply_labels(covid,
                      cases = "New cases by specimen date",
                      deaths.28 = "New deaths 28 days after diagnosis, by death date",
                      deaths.60 = "New deaths 60 days after diagnosis, by death date",
                      reinf = "New reinfections by specimen date")

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
  geom_line(aes(y = cases))+
  annotate("rect", xmin = 132, xmax = 158,
           ymin = -500, ymax = max(covid$cases)+500,
           fill = "red", alpha = 0.1)+
  annotate("rect", xmin = 183, xmax = 276,
           ymin = -500, ymax = max(covid$cases)+500,
           fill = "red", alpha = 0.1)+
  scale_y_continuous("New Cases", expand = c(0,0))+
  xlab("Time (days)")+
  theme_light()+
  theme(plot.background = element_rect(fill = "transparent", colour = NA))

ggplot(covid, aes(x = t))+
  geom_line(aes(y = log(cases)))+
  annotate("rect", xmin = 132, xmax = 158,
           ymin = -0.2, ymax = max(log(covid$cases))+0.2,
           fill = "red", alpha = 0.1)+
  annotate("rect", xmin = 183, xmax = 276,
           ymin = -0.2, ymax = max(log(covid$cases))+0.2,
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

# compare variables against cases ====

vars.plot <- c("rain", "mean.temp", "min.temp", "max.temp")

plot.list <- list()

for (i in 1:length(vars.plot)) {
  
  plot.list[[vars.plot[i]]][["no.lag"]] <- 
    ggplot(covid, aes(y = log(cases)))+
    geom_point(aes_string(x = vars.plot[i]))
  
  lags <- c(2, 7, 10, 28)
  
  for (j in 1:length(lags)) {
    
    plot.list[[vars.plot[i]]][[paste0("lag.", lags[j])]] <-
      ggplot(covid, aes(y = log(cases)))+
      geom_point(aes_string(x = paste0(vars.plot[i], ".lag.", lags[j])))
    
  }
  
  
}


# create splines with bs() base spline from package "spline"
bs()
# knots -> give as a vector all the point you want the slope to change 


# Quick lm() playaround ====

covid.comp <- covid[complete.cases(covid),]

lm.fit1 <- lm(log(cases) ~ t + sin(2*pi*t/7) + cos(2*pi*t/7)+
                lockdown + rain.lag.7 + max.temp.lag.7 + min.temp.lag.7+
                vacc.2nd + vacc.3rd,
              data = covid.comp)
summary(lm.fit1)

covid.comp$pred.cases <- predict(lm.fit1)


ggplot(covid.comp, aes(x = t))+
  geom_line(aes(y = log(cases)), col = "black")+
  geom_line(aes(y = pred.cases), col = "red")

ggplot(covid, aes(x = t))+
  geom_ribbon(aes(ymin = min.temp, ymax = max.temp), stat = "identity",
              fill = "red", alpha = 0.5)+
  geom_line(aes(y = mean.temp), col = "black")+
    ylab("Temperature")

# show seasonality within a week 

beta.hat.lm.1 <- coef(lm.fit1)

week.t <- 1:14

seasonality.lm1 <- beta.hat.lm.1[2]*week.t +
                   beta.hat.lm.1[3]*sin(2*pi*week.t/7)+
                   beta.hat.lm.1[4]*cos(2*pi*week.t/7)

week.seasonality <- data.frame(
  day = week.t,
  cases.pred = seasonality.lm1
)

ggplot(week.seasonality, aes(x = day, y = cases.pred))+
  geom_line()
