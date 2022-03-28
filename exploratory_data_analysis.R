# Load data and packages ====

library(ggplot2)
library(lubridate)
library(ggpubr)

covid <- read.csv("data/covid_data.csv")

covid$date <- ymd(covid$date)

## add labels to variables ----

library(expss)

covid <- apply_labels(covid,
                      cases = "New cases by specimen date",
                      deaths.28 = "New deaths 28 days after diagnosis, by death date",
                      deaths.60 = "New deaths 60 days after diagnosis, by death date",
                      reinf = "New reinfections by specimen date")



## set my_theme() -----

my_theme <- function(){
  
  font <- "serif"
  
  theme_light() %+replace%
    
    theme(
      text = element_text(family = font)
    )   
}

# 1. Exploratory analysis of variables over time ====

## a. COVID cases ----

covid$incidence <- covid$cases/covid$pop

p1 <- ggplot(covid, aes(x = date, y = log(incidence)))+
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

ggsave("output/fig_1.png", p1, device = "png", units = "cm",
       height = 7.48, width = 15.89)

ggplot(covid, aes(x = date, y = log(reinf+1)))+
  geom_line()

# Vaccinations 

ggplot(covid, aes(x = date))+
  geom_line(aes(y = vacc.2nd), col = "purple")+
  geom_line(aes(y = vacc.3rd), col = "orange")

p2 <- ggplot(covid, aes(x = date))+
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
  
ggsave("output/fig_2.png", p2, device = "png", units = "cm",
       height = 7.48, width = 15.89)

covid$date[which(covid$vacc.2nd != 0)]

min(covid$date[which(covid$vacc.2nd != 0)])

covid$date[which(covid$vacc.2nd==max(covid$vacc.2nd))]

covid$date[which(covid$vacc.2nd==max(covid$vacc.2nd[covid$t<220]))]

min(covid$date[which(covid$vacc.3rd>12500)])

## b. Rain -----

ggplot(covid, aes(x = date, y = rain))+
  geom_line()

p3 <- ggplot(covid, aes(x = date, y = rain))+
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

ggsave("output/fig_3.png", p3, device = "png", units = "cm",
       height = 7.48, width = 15.89)



## c. Temperature ----

p4 <- ggplot(covid, aes(x = date))+
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

ggsave("output/fig_4.png", p4, device = "png", units = "cm",
       height = 7.48, width = 15.89)

# 2. Compare variables against log(incidence) ====

vars.plot <- c("rain", "mean.temp", "min.temp", "max.temp")

plot.list <- list()

for (i in 1:length(vars.plot)) {
  
  plot.list[[vars.plot[i]]][["no.lag"]] <- 
    ggplot(covid, aes(y = log(incidence)))+
    geom_point(aes_string(x = vars.plot[i], col = "lockdown"))+
    geom_smooth(method = lm, aes_string(x = vars.plot[i]))+
    geom_smooth(col = "red",se = FALSE,
                linetype = "dashed", size = 0.5,
                aes_string(x = vars.plot[i]))
  
  lags <- c(2, 6, 14)
  
  for (j in 1:length(lags)) {
    
    plot.list[[vars.plot[i]]][[paste0("lag.", lags[j])]] <-
      ggplot(covid, aes(y = log(incidence)))+
      geom_point(aes_string(x = paste0(vars.plot[i], ".lag.", lags[j]),
                            col = "lockdown"))+
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

# 2.b) compare variables against RESIDUALS of basic model ====

covid.complete <- covid[complete.cases(covid),]

lm.fit0 <- lm(log(incidence) ~ sin(2*pi*t/7) + cos(2*pi*t/7)+
                lockdown + vacc.2nd + vacc.3rd, 
              data = covid.complete)

covid.complete$lm.resid <- lm.fit0$residuals

vars.plot <- c("rain", "mean.temp", "min.temp", "max.temp")

plot.resid <- list()

for (i in 1:length(vars.plot)) {
  
  plot.resid[[vars.plot[i]]][["no.lag"]] <- 
    ggplot(covid.complete, aes(y = lm.resid))+
    geom_point(aes_string(x = vars.plot[i]))+
    geom_smooth(method = lm, aes_string(x = vars.plot[i]))+
    geom_smooth(col = "red",se = FALSE,
                linetype = "dashed", size = 0.5,
                aes_string(x = vars.plot[i]))
  
  lags <- c(2, 6, 14)
  
  for (j in 1:length(lags)) {
    
    plot.resid[[vars.plot[i]]][[paste0("lag.", lags[j])]] <-
      ggplot(covid.complete, aes(y = lm.resid))+
      geom_point(aes_string(x = paste0(vars.plot[i], ".lag.", lags[j])))+
      geom_smooth(method = lm,
                  aes_string(x = paste0(vars.plot[i], ".lag.", lags[j])))+
      geom_smooth(col = "red",se = FALSE,
                  linetype = "dashed", size = 0.5,
                  aes_string(x = paste0(vars.plot[i], ".lag.", lags[j])))
    
  }
  
  
}

plot.resid$rain

A <- plot.resid$rain$no.lag + labs(x = "Daily estimated rainfall, mm",
                                   y = "Residual") +
  my_theme()
B <- plot.resid$rain$lag.2+ labs(x = "Daily estimated rainfall, mm",
                                 y = "Residual") +
  my_theme()
C <- plot.resid$rain$lag.6+ labs(x = "Daily estimated rainfall, mm",
                                 y = "Residual") +
  my_theme()
D <- plot.resid$rain$lag.14+ labs(x = "Daily estimated rainfall, mm",
                                  y = "Residual") +
  my_theme()

p5 <- ggpubr::ggarrange(A, B, C, D, labels = "AUTO", 
                  font.label = list(family = "serif"),
                  hjust = -0.3)

ggsave("output/fig_5.png", p5, device = "png", units = "cm",
       height = 12, width = 15.88)

ggsave("output/fig_4.png", p4, device = "png", units = "cm",
       height = 7.48, width = 15.89)
plot.resid$mean.temp

plot.resid$min.temp

plot.resid$max.temp


# 3. Build model ====

covid.complete <- covid[complete.cases(covid),]

env.vars <- c("rain", "rain.lag.2", "rain.lag.6", "rain.lag.14",
              "mean.temp", "mean.temp.lag.2", "mean.temp.lag.6", "mean.temp.lag.14",
              "min.temp", "min.temp.lag.2", "min.temp.lag.6", "min.temp.lag.14",
              "max.temp", "max.temp.lag.2", "max.temp.lag.6", "max.temp.lag.14")

compare.models <- data.frame(
  env.var = env.vars,
  coef = rep(NA, length(env.vars)),
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
  
  
}

knitr::kable(compare.models, format = 'html')

compare.models


## Model stage 2 -----

lm.fit2 <- lm(log(incidence) ~ sin(2*pi*t/7) + cos(2*pi*t/7)+
                lockdown + vacc.2nd + vacc.3rd +
                max.temp.lag.6 + min.temp.lag.6 + mean.temp.lag.6,
              data = covid.complete)

summary(lm.fit2)



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
                lockdown,
              data = covid.comp)
summary(lm.fit1)$coefficients[5,1]
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