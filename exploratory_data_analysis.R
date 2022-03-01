# Load data and packages ====

library(ggplot2)
library(lubridate)

covid <- read.csv("data/covid_data.csv")

covid$date <- ymd(covid$date)
covid$newPeopleVaccinatedSecondDoseByVaccinationDate[is.na(covid$newPeopleVaccinatedSecondDoseByVaccinationDate)] <- 0
covid$newPeopleVaccinatedThirdInjectionByVaccinationDate[is.na(covid$newPeopleVaccinatedThirdInjectionByVaccinationDate)] <- 0

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
           ymin = 0, ymax = max(covid$newCasesBySpecimenDate),
           fill = "red", alpha = 0.1)+
  annotate("rect", xmin = 183, xmax = 276,
           ymin = 0, ymax = max(covid$newCasesBySpecimenDate),
           fill = "red", alpha = 0.1)+
  scale_y_continuous("log New Cases", expand = c(0,0))

ggplot(covid, aes(x = t))+
  geom_line(aes(y = log(newCasesBySpecimenDate)))+
  annotate("rect", xmin = 132, xmax = 158,
           ymin = 0, ymax = max(log(covid$newCasesBySpecimenDate)),
           fill = "red", alpha = 0.1)+
  annotate("rect", xmin = 183, xmax = 276,
           ymin = 0, ymax = max(log(covid$newCasesBySpecimenDate)),
           fill = "red", alpha = 0.1)+
  scale_y_continuous("log New Cases", expand = c(0,0))
  

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

# create splines with bs() base spline from package "spline"
bs()
# knots -> give as a vector all the point you want the slope to change 
