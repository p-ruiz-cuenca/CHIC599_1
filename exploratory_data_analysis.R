# Load data and packages ====

library(ggplot2)
library(lubridate)

covid <- read.csv("data/covid_data.csv")

covid$date <- as.numeric(ymd(covid$date))
covid$date <- covid$date-min(covid$date)+1

((day(covid$date)-27)+(ifelse(month(covid$date)==2, 28,
                             ifelse(month(covid$date)%in%c(1,3,5,7,8,10,12),
                                    31, 30)))*(month(covid$date)-6)+
    ifelse(leap_year(year(covid$date)),
           366, 365)*(year(covid$date)-2020))

[188]

ifelse(leap_year(year(covid$date)),
       366, 365)

leap_year(year(covid$date))[189]


ifelse(month(covid$date)==2, 28,
       ifelse(month(covid$date)%in%c(1,3,5,7,8,10,12),
              31, 30))

ifelse(month(covid$date)%in%c(1,3,5,7,8,10,12),
       31, 30)

ifelse(month(covid$date)==1 | month(covid$date)==3,
       31, 30)

# create splines with bs() base spline from package "spline"
bs()
# knots -> give as a vector all the point you want the slope to change 

  
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