# 1.Load packages ====
library(reshape2)
library(stringr)
library(lubridate)
library(ggplot2)
library(ggpubr)

# 2.Load data ====

## a. Temperature data ----

clean.temp.data <- function(data, temp.name){
  
  require(reshape2)
  require(lubridate)
  
  temp <- read.fwf(data,
                   widths = rep(5, 14),
                   header = FALSE,
                   col.names = c("Year", "Day",
                                 "Jan", "Feb", "Mar", "Apr",
                                 "May", "Jun", "Jul", "Aug",
                                 "Sep", "Oct", "Nov", "Dec"))
  
  
  # change -999 to NA
  temp[temp == -999] <- NA
  
  # wide to long
  temp <- melt(temp, id = c("Year", "Day"))
  names(temp)[c(3,4)] <- c("Month", temp.name)
  
  # change month to factor
  
  temp$Month <- factor(temp$Month,
                       levels = c("Jan", "Feb", "Mar", "Apr",
                                  "May", "Jun", "Jul", "Aug",
                                  "Sep", "Oct", "Nov", "Dec"),
                       ordered = TRUE)
  
  # introduce decimal points 
  
  temp[[temp.name]] <- (temp[[temp.name]])/10
  
  
  # order data frame by Year, Month and then Day 
  
  temp <- temp[order(temp$Year,
                     temp$Month,
                     temp$Day),]
  
  # Create date variable combining Year, Month and Day 
  
  temp$date <- paste(temp$Year, as.numeric(temp$Month), temp$Day,
                     sep = "-")
  
  temp$date <- ymd(temp$date)
  
  return(temp)
  
}

mean.temp <- clean.temp.data(data = "raw_data/daily_HadCET_1772_2022.txt",
                             temp.name = "mean.temp")

min.temp <- clean.temp.data(data = "raw_data/daily_min_HadCET_1878_2022.txt",
                            temp.name = "min.temp")

max.temp <- clean.temp.data(data = "raw_data/daily_max_HadCET_1878_2022.txt",
                            temp.name = "max.temp")

## b. Rainfall data ----

rain <- read.fwf("raw_data/daily_rainfall_NWEP_HadUKP.txt", 
                 widths = c(5, 5, rep(7, 31)),
                 header = FALSE)

# rename columns
names(rain) <- c("Year", "Month", seq(1, 31, by = 1))

# change -99.99 to NA

rain[rain==-99.99] <- NA

# wide to long 

rain <- melt(rain, id = c("Year", "Month"))

names(rain)[c(3,4)] <- c("Day", "rain")

# order data by Year, Month and Day 

rain <- rain[order(rain$Year,
                   rain$Month,
                   rain$Day),]

# create date column

rain$date <- paste(rain$Year, rain$Month, rain$Day,
                   sep = "-")
rain$date <- ymd(rain$date)

## c. COVID data ----

covid <- read.csv("raw_data/region_2022-02-24.csv")
reinf <- read.csv("raw_data/reinfections.csv")

covid$date <- ymd(covid$date)
reinf$date <- ymd(reinf$date)

covid <- merge(covid, reinf[,c(4,5)], by = "date")

# change NA's in vaccination variables to 0

covid$newPeopleVaccinatedSecondDoseByVaccinationDate[is.na(covid$newPeopleVaccinatedSecondDoseByVaccinationDate)] <- 0 
covid$newPeopleVaccinatedThirdInjectionByVaccinationDate[is.na(covid$newPeopleVaccinatedThirdInjectionByVaccinationDate)] <- 0


## d. Population data ----

# copied from "raw_data/ukpopestimatesmid2020on2021geography.xls"
# sheet "MYE2 - Persons"

# population estimate for North-West England for mid-2020 = 7,367,456
covid$pop <- rep(7367456, length(covid$date))

## e. Lockdown variable ----

## e. Create binary lockdown variables -----

covid$lockdown <- rep(0, length(covid$date))

# second lockdown
covid$lockdown[covid$date > ymd("2020-11-04") & 
                 covid$date < ymd("2020-12-02")] <- 1

# third lockdown 
covid$lockdown[covid$date > ymd("2020-12-25") & 
                 covid$date < ymd("2021-03-30")] <- 1

## f. merge environmental vars with covid data ----

covid <- merge(covid, rain[,c(4,5)], by = "date")
covid <- merge(covid, mean.temp[,c(4,5)], by = "date")
covid <- merge(covid, min.temp[,c(4,5)], by = "date")
covid <- merge(covid, max.temp[,c(4,5)], by = "date")

# Create lagged variables 

covid$t <- as.numeric(covid$date)-(min(as.numeric(covid$date)))+1

create.lag.var <- function(x,lag,data) {
  tlag <- data$t-lag
  ind.lag <- sapply(tlag,function(k) {
    out <- which((data$t-k)==0)
    if(length(out)==0) out <- NA
    return(out)
  })
  x[ind.lag]
}

# for() loop to create lagged variables

env.vars <- c("rain", "mean.temp", "min.temp", "max.temp")

for (i in 1:length(env.vars)) {
  
  lags <- c(2, 6, 14)
  
  for (j in 1:length(lags)) {
    
    covid[[paste0(env.vars[i], ".lag.", lags[j])]] <- create.lag.var(x = covid[[env.vars[i]]],
                                                                     lag = lags[j],
                                                                     data = covid)
    
  }
  
}

# Change NA's from lagged variables into actual data from environmental data sets
# Done by looking up dates where NA's are present
# finding the dates they correspond to
# and finding the corresponding environmental data for those dates

covid$rain.lag.2[is.na(covid$rain.lag.2)] <- rain$rain[which(rain$date%in%(covid$date[is.na(covid$rain.lag.2)]-2))]
covid$rain.lag.6[is.na(covid$rain.lag.6)] <- rain$rain[which(rain$date%in%(covid$date[is.na(covid$rain.lag.6)]-6))]
covid$rain.lag.14[is.na(covid$rain.lag.14)] <- rain$rain[which(rain$date%in%(covid$date[is.na(covid$rain.lag.14)]-14))]

covid$mean.temp.lag.2[is.na(covid$mean.temp.lag.2)] <- mean.temp$mean.temp[which(mean.temp$date%in%(covid$date[is.na(covid$mean.temp.lag.2)]-2))]
covid$mean.temp.lag.6[is.na(covid$mean.temp.lag.6)] <- mean.temp$mean.temp[which(mean.temp$date%in%(covid$date[is.na(covid$mean.temp.lag.6)]-6))]
covid$mean.temp.lag.14[is.na(covid$mean.temp.lag.14)] <- mean.temp$mean.temp[which(mean.temp$date%in%(covid$date[is.na(covid$mean.temp.lag.14)]-14))]

covid$min.temp.lag.2[is.na(covid$min.temp.lag.2)] <- min.temp$min.temp[which(min.temp$date%in%(covid$date[is.na(covid$min.temp.lag.2)]-2))]
covid$min.temp.lag.6[is.na(covid$min.temp.lag.6)] <- min.temp$min.temp[which(min.temp$date%in%(covid$date[is.na(covid$min.temp.lag.6)]-6))]
covid$min.temp.lag.14[is.na(covid$min.temp.lag.14)] <- min.temp$min.temp[which(min.temp$date%in%(covid$date[is.na(covid$min.temp.lag.14)]-14))]

covid$max.temp.lag.2[is.na(covid$max.temp.lag.2)] <- max.temp$max.temp[which(max.temp$date%in%(covid$date[is.na(covid$max.temp.lag.2)]-2))]
covid$max.temp.lag.6[is.na(covid$max.temp.lag.6)] <- max.temp$max.temp[which(max.temp$date%in%(covid$date[is.na(covid$max.temp.lag.6)]-6))]
covid$max.temp.lag.14[is.na(covid$max.temp.lag.14)] <- max.temp$max.temp[which(max.temp$date%in%(covid$date[is.na(covid$max.temp.lag.14)]-14))]


# 3. Save as csv file ====

# change variable names for ease of use 

names(covid)[5:10] <- c("cases", "deaths.28", "deaths.60", "vacc.2nd", "vacc.3rd",
                        "reinf")

# re-arrange columns 

names(covid)[c(1,15,2:4, 28, 5:10, 29, 11, 16:18,
               12, 19:21, 13, 22:24, 14, 25:27)]

covid <- covid[,c(1,15,2:4, 28, 5:10, 29, 11, 16:18,
                  12, 19:21, 13, 22:24, 14, 25:27)]

# save as csv 
write.csv(covid, file = "data/covid_data.csv", na = "NA",
          row.names = FALSE)

# 4. Exploratory analysis of variables over time ====

my_theme <- function(){
  
  font <- "serif"
  
  theme_light() %+replace%
    
    theme(
      text = element_text(family = font)
    )   
}

## a. COVID cases ----

covid <- read.csv("data/covid_data.csv")

covid$date <- ymd(covid$date)

ggplot(covid, aes(x = date, y = log(incidence)))+
  geom_line() +
  labs(x = "Date", y = "log(Incidence)")+
  annotate("rect", xmin = ymd("2020-11-04"), xmax = ymd("2020-12-02"),
           ymin = -12, ymax = max(log(covid$incidence))+0.2,
           fill = "red", alpha = 0.1)+
  annotate("rect", xmin = ymd("2020-12-25"), xmax = ymd("2021-03-30"),
           ymin = -12, ymax = max(log(covid$incidence))+0.2,
           fill = "red", alpha = 0.1)+
  scale_y_continuous(expand = c(0,0))+
  scale_x_date(date_labels = "%b-%y",
               breaks = ymd(c("2020-07-01", "2020-09-01", "2020-11-01",
                              "2021-01-01", "2021-03-01", "2021-05-01",
                              "2021-07-01", "2021-09-01", "2021-11-01",
                              "2022-01-01", "2022-03-01")),
               date_minor_breaks = "1 months", 
               limits = c(min(covid$date), max(covid$date)))+
  my_theme()+
  theme(axis.text.x=element_text(angle=20, hjust=1, size = 8))

## b. Vaccinations ----

ggplot(covid, aes(x = date))+
  geom_line(aes(y = log((vacc.2nd+1)/pop), col = "2nd Vaccination"))+
  geom_line(aes(y = log((vacc.3rd+1)/pop), col = "3rd Vaccination"))+
  geom_line(aes(y = log(incidence), col = "COVID-19 incidence"))+
  labs(x = "Date", y = "log(Incidence)", col = "Legend")+
  scale_x_date(date_labels = "%b-%y",
               breaks = ymd(c("2020-07-01", "2020-09-01", "2020-11-01",
                              "2021-01-01", "2021-03-01", "2021-05-01",
                              "2021-07-01", "2021-09-01", "2021-11-01",
                              "2022-01-01", "2022-03-01")),
               date_minor_breaks = "1 months", 
               limits = c(min(covid$date), max(covid$date)))+
  scale_color_manual(values = c("COVID-19 incidence" = "black",
                                "2nd Vaccination" = "purple",
                                "3rd Vaccination" = "orange"))+
  my_theme()+
  guides(color = guide_legend(byrow = TRUE))+
  theme(axis.text.x=element_text(angle=20, hjust=1, size = 8),
        legend.title = element_text(size = 8),
        legend.text = element_text(size=8),
        legend.spacing.y = unit(0.5, "mm"))

## c. Rain ----

ggplot(covid, aes(x = date, y = rain))+
  geom_line()+
  labs(x = "Date", y = "Daily rainfall (mm)")+
  scale_x_date(date_labels = "%b-%y",
               breaks = ymd(c("2020-07-01", "2020-09-01", "2020-11-01",
                              "2021-01-01", "2021-03-01", "2021-05-01",
                              "2021-07-01", "2021-09-01", "2021-11-01",
                              "2022-01-01", "2022-03-01")),
               date_minor_breaks = "1 months", 
               limits = c(min(covid$date), max(covid$date)))+
  my_theme()+
  theme(axis.text.x=element_text(angle=20, hjust=1, size = 8))

## d. Temperature ----

ggplot(covid, aes(x = date))+
  geom_ribbon(aes(ymax = max.temp, ymin = min.temp), fill = "blue",
              alpha = 0.3)+
  geom_line(aes(y = mean.temp))+
  labs(x = "Date", y = "Daily Temperature (°C)")+
  scale_x_date(date_labels = "%b-%y",
               breaks = ymd(c("2020-07-01", "2020-09-01", "2020-11-01",
                              "2021-01-01", "2021-03-01", "2021-05-01",
                              "2021-07-01", "2021-09-01", "2021-11-01",
                              "2022-01-01", "2022-03-01")),
               date_minor_breaks = "1 months", 
               limits = c(min(covid$date), max(covid$date)))+
  my_theme()+
  theme(axis.text.x=element_text(angle=20, hjust=1, size = 8))

# 5. compare variables against RESIDUALS of basic model ====

## a. Basic model ----

covid.complete <- covid[complete.cases(covid),]

lm.fit0 <- lm(log(incidence) ~ sin(2*pi*t/7) + cos(2*pi*t/7)+
                lockdown + vacc.2nd + vacc.3rd, 
              data = covid.complete)

covid.complete$lm.resid <- lm.fit0$residuals

## b. Plot variables against residuals ----

vars.plot <- c("rain", "mean.temp", "min.temp", "max.temp")

plot.resid <- list()

for (i in 1:length(vars.plot)) {
  
  plot.resid[[vars.plot[i]]][["no.lag"]] <- 
    ggplot(covid.complete, aes(y = lm.resid))+
    geom_point(aes_string(x = vars.plot[i]),
               alpha = 0.6)+
    geom_smooth(method = lm, aes_string(x = vars.plot[i]),
                fill = "#8C89FB")
  
  
  lags <- c(2, 6, 14)
  
  for (j in 1:length(lags)) {
    
    plot.resid[[vars.plot[i]]][[paste0("lag.", lags[j])]] <-
      ggplot(covid.complete, aes(y = lm.resid))+
      geom_point(aes_string(x = paste0(vars.plot[i], ".lag.", lags[j])),
                 alpha = 0.6)+
      geom_smooth(method = lm,
                  aes_string(x = paste0(vars.plot[i], ".lag.", lags[j])),
                  fill = "#8C89FB")
    
    
  }
  
  
}

plot.resid$rain

plot.resid$mean.temp

plot.resid$min.temp

plot.resid$max.temp

# 6. Build final model ====

covid.complete <- covid[complete.cases(covid),]

env.vars <- c("rain", "rain.lag.2", "rain.lag.6", "rain.lag.14",
              "mean.temp", "mean.temp.lag.2", "mean.temp.lag.6", "mean.temp.lag.14",
              "min.temp", "min.temp.lag.2", "min.temp.lag.6", "min.temp.lag.14",
              "max.temp", "max.temp.lag.2", "max.temp.lag.6", "max.temp.lag.14")

compare.models <- data.frame(
  env.var = env.vars,
  coef = rep(NA, length(env.vars)),
  lower = rep(NA, length(env.vars)),
  upper = rep(NA, length(env.vars)),
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
  
  compare.models$coef[i] <- exp(summary(lm.fit1)$coefficients[7,1])
  
  compare.models$coef[i] <- round(compare.models$coef[i], digits = 3)
  
  compare.models$lower[i] <- exp(summary(lm.fit1)$coefficients[7,1] - 
                                   qnorm(0.975)*summary(lm.fit1)$coefficients[7,2])
  
  compare.models$lower[i] <- round(compare.models$lower[i], digits = 3)
  
  compare.models$upper[i] <- exp(summary(lm.fit1)$coefficients[7,1] + 
                                   qnorm(0.975)*summary(lm.fit1)$coefficients[7,2])
  
  compare.models$upper[i] <- round(compare.models$upper[i], digits = 3)
  
  
}

compare.models

# 7. Check residual temporal correlation =====

source("auxiliary_function.R")

lm.fit.final <- lm(log(incidence) ~ sin(2*pi*t/7) + cos(2*pi*t/7)+
                     lockdown + vacc.2nd + vacc.3rd +
                     max.temp.lag.6,
                   data = covid.complete)

vari.lm.final <- vari.time(time = covid.complete$t,
                           data = residuals(lm.fit.final),
                           uvec = seq(1, 90, by = 1))

env.vari.1 <- vari.env(vari.lm.final,
                       time = covid.complete$t,
                       data = residuals(lm.fit.final),
                       nsim = 10000)

# Combine in data.frame for ease of plotting

vari.covid <- data.frame(
  u = vari.lm.final$u,
  v = vari.lm.final$v,
  v.lower = env.vari.1$v.lower,
  v.upper = env.vari.1$v.upper
)

ggplot(vari.covid, aes(x = u))+
  geom_line(aes(y = v), col = "black")+
  geom_line(aes(y = v.upper), col = "black", linetype = "dashed", size = 0.3)+
  geom_line(aes(y = v.lower), col = "black", linetype = "dashed", size = 0.3)+
  labs(x = "Time separation (days)", y = "Variogram")+
  my_theme()