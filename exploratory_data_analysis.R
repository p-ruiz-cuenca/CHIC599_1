# Load data and packages ====

library(ggplot2)
library(lubridate)

covid <- read.csv("data/covid_data.csv")

covid$date <- ymd(covid$date)

# add labels to variables 

library(expss)

covid <- apply_labels(covid,
                      cases = "New cases by specimen date",
                      deaths.28 = "New deaths 28 days after diagnosis, by death date",
                      deaths.60 = "New deaths 60 days after diagnosis, by death date",
                      reinf = "New reinfections by specimen date")


# 1. Exploratory analysis of variables over time ====

## a. COVID cases ----

covid$incidence <- covid$cases/covid$pop

ggplot(covid, aes(x = date, y = log(incidence)))+
  geom_line()

ggplot(covid, aes(x = date, y = log(reinf+1)))+
  geom_line()

# Vaccinations 

ggplot(covid, aes(x = date))+
  geom_line(aes(y = vacc.2nd), col = "red")+
  geom_line(aes(y = vacc.3rd), col = "blue")

## b. Rain -----

ggplot(covid, aes(x = date, y = rain))+
  geom_line()

## c. Temperature ----

ggplot(covid, aes(x = date))+
  geom_ribbon(aes(ymax = max.temp, ymin = min.temp), fill = "red",
              alpha = 0.3)+
  geom_line(aes(y = mean.temp))

# 2. Compare variables against log(incidence) ====

vars.plot <- c("rain", "mean.temp", "min.temp", "max.temp")

plot.list <- list()

for (i in 1:length(vars.plot)) {
  
  plot.list[[vars.plot[i]]][["no.lag"]] <- 
    ggplot(covid, aes(y = log(incidence)))+
    geom_point(aes_string(x = vars.plot[i]))+
    geom_smooth(method = lm, aes_string(x = vars.plot[i]))+
    geom_smooth(col = "red",se = FALSE,
                linetype = "dashed", size = 0.5,
                aes_string(x = vars.plot[i]))
  
  lags <- c(2, 6, 14)
  
  for (j in 1:length(lags)) {
    
    plot.list[[vars.plot[i]]][[paste0("lag.", lags[j])]] <-
      ggplot(covid, aes(y = log(incidence)))+
      geom_point(aes_string(x = paste0(vars.plot[i], ".lag.", lags[j])))+
      geom_smooth(method = lm,
                  aes_string(x = paste0(vars.plot[i], ".lag.", lags[j])))+
      geom_smooth(col = "red",se = FALSE,
                  linetype = "dashed", size = 0.5,
                  aes_string(x = paste0(vars.plot[i], ".lag.", lags[j])))
    
  }
  
  
}

plot.list$rain

plot.list$mean.temp

plot.list$min.temp

plot.list$max.temp

# 3. Build model ====

covid.complete <- covid[complete.cases(covid),]

env.vars <- c("rain", "rain.lag.2", "rain.lag.6", "rain.lag.14",
              "mean.temp", "mean.temp.lag.2", "mean.temp.lag.6", "mean.temp.lag.14",
              "min.temp", "min.temp.lag.2", "min.temp.lag.6", "min.temp.lag.14",
              "max.temp", "max.temp.lag.2", "max.temp.lag.6", "max.temp.lag.14")

compare.models <- data.frame(
  env.var = env.vars,
  p.value = rep(NA, length(env.vars)),
  signif = rep(NA, length(env.vars)),
  aic = rep(NA, length(env.vars)))


for (i in 1:length(env.vars)) {
  
  lm.fit1 <- lm(as.formula(paste0("log(incidence) ~ sin(2*pi*t/7) + cos(2*pi*t/7)+
                lockdown + vacc.2nd + vacc.3rd +", env.vars[i])),
                data = covid.complete)
  
  compare.models$p.value[i] <- summary(lm.fit1)$coefficients[7,4]
  
  compare.models$aic[i] <- AIC(lm.fit1)
  
  if (compare.models$p.value[i] > 0.1){
    compare.models$signif[i] <- c("-")
  } else if(compare.models$p.value[i] < 0.1 &
            compare.models$p.value[i] > 0.05){
    compare.models$signif[i] <- c(".")
  } else if(compare.models$p.value[i] < 0.05 &
            compare.models$p.value[i] > 0.01){
    compare.models$signif[i] <- c("*")
  } else if(compare.models$p.value[i] < 0.01 & 
            compare.models$p.value[i] > 0.001){
    compare.models$signif[i] <- c("**")
  } else if(compare.models$p.value[i] < 0.001){
    compare.models$signif[i] <- c("***")
  }
  
  compare.models$p.value[i] <- round(compare.models$p.value[i], digits = 3)  
  
}

knitr::kable(compare.models, format = 'html')

compare.models


###############################################################################

splines::bs()

# create splines with bs() base spline from package "spline"
bs()
# knots -> give as a vector all the point you want the slope to change 

n <- 10
d <- data.frame(x = 1:n, y = rnorm(n))
ggplot(d,aes(x,y)) + geom_point() + 
  geom_line(data=data.frame(spline(d, n=n*10)))

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


# 3. Is there seasonality in covid incidence?

covid$week.day <- wday(covid$date, label = TRUE, 
                       week_start = getOption("lubridate.week.start", 1))

covid$week.num <- (isoweek(covid$date)-25)+(52*(year(covid$date)-2020))

covid %>% 
  group_by(week.num, week.day) %>% 
  summarise(cases = cases) %>% 
  ungroup() %>% 
  group_by(week.num) %>% 
  mutate(prop.cases = cases/max(cases)) %>% 
  ggplot()+
  geom_line(aes(x = week.day, y = prop.cases, group = week.num),
            alpha = 0.2)

ggplot(covid, aes(x = week.day, y = log(incidence)))+
  geom_line(aes(group = week.num), alpha = 0.2)