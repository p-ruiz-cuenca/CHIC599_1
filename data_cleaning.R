# 1.Load data ====
library(reshape2)
library(stringr)
library(lubridate)

## a. Temperature data ------
# units = degrees C

# Create function to import temperature data

# use the second function, the first is more tedious to introduce decimal points
#clean.temp.data <- function(data, temp.name){
# 
# require(reshape2)
# require(stringr)
# 
# temp <- read.fwf(data,
#                  widths = rep(5, 14),
#                  header = FALSE,
#                  col.names = c("Year", "Day",
#                                "Jan", "Feb", "Mar", "Apr",
#                                "May", "Jun", "Jul", "Aug",
#                                "Sep", "Oct", "Nov", "Dec"))
# 
# 
# # change -999 to NA
# temp[temp == -999] <- NA
# 
# # wide to long
# temp <- melt(temp, id = c("Year", "Day"))
# names(temp)[c(3,4)] <- c("Month", temp.name)
# 
# # change month to factor
# 
# temp$Month <- factor(temp$Month,
#                      levels = c("Jan", "Feb", "Mar", "Apr",
#                                 "May", "Jun", "Jul", "Aug",
#                                 "Sep", "Oct", "Nov", "Dec"),
#                      ordered = TRUE)
# 
# # introduce decimal points 
# 
# for (i in 1:length(temp[[temp.name]])) {
#    
#    if (nchar(abs(temp[[temp.name]][i]))==1 | is.na(temp[[temp.name]][i])==TRUE) {
#      
#      temp[[temp.name]][i] <- temp[[temp.name]][i]
#      
#    } else { 
#      
#      temp[[temp.name]][i] <- as.numeric(paste0(str_sub(temp[[temp.name]][i],
#                                                          1, (nchar(temp[[temp.name]][i])-1)),
#                                                  ".",
#                                                  str_sub(temp[[temp.name]][i], nchar(temp[[temp.name]][i]),
#                                                          nchar(temp[[temp.name]][i]))))
#      
#    }
#    
#  }
# 
# # order data frame by Year, Month and then Day 
# 
# temp <- temp[order(temp$Year,
#                    temp$Month,
#                    temp$Day),]
# 
# 
# return(temp)
# 
#}

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


# This is now included in the function above

#mean.temp$date <- paste(mean.temp$Year, as.numeric(mean.temp$Month), mean.temp$Day,
#                        sep = "-")

#mean.temp$date <- ymd(mean.temp$date)

#min.temp$date <- paste(min.temp$Year, as.numeric(min.temp$Month), min.temp$Day,
#                        sep = "-")

#min.temp$date <- ymd(min.temp$date)

#max.temp$date <- paste(max.temp$Year, as.numeric(max.temp$Month), max.temp$Day,
#                        sep = "-")

#max.temp$date <- ymd(max.temp$date)



# plot data

plot(1:length(mean.temp$mean.temp[mean.temp$Year > 2019]),
     mean.temp$mean.temp[mean.temp$Year > 2019],
     type = "l")

matplot(1:length(mean.temp$mean.temp[mean.temp$Year > 2020]),
        cbind(min.temp$min.temp[min.temp$Year>2020],
              mean.temp$mean.temp[mean.temp$Year>2020],
              max.temp$max.temp[max.temp$Year>2020]),
        type = "l",
        lty = c("dashed", "solid", "dashed"),
        col = 1,
        xlab = "Day",
        ylab = "Temp")

## b. Rainfall data -----
# units = mm

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


### merge covid data with environmental data ----

covid <- merge(covid, rain[,c(4,5)], by = "date")
covid <- merge(covid, mean.temp[,c(4,5)], by = "date")
covid <- merge(covid, min.temp[,c(4,5)], by = "date")
covid <- merge(covid, max.temp[,c(4,5)], by = "date")

### create lagged variables ----

# Create "t" variable 
covid$t <- as.numeric(covid$date)-(min(as.numeric(covid$date)))+1

# create function 

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
  
  lags <- c(2, 7, 10, 28)
  
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
covid$rain.lag.7[is.na(covid$rain.lag.7)] <- rain$rain[which(rain$date%in%(covid$date[is.na(covid$rain.lag.7)]-7))]
covid$rain.lag.10[is.na(covid$rain.lag.10)] <- rain$rain[which(rain$date%in%(covid$date[is.na(covid$rain.lag.10)]-10))]
covid$rain.lag.28[is.na(covid$rain.lag.28)] <- rain$rain[which(rain$date%in%(covid$date[is.na(covid$rain.lag.28)]-28))]

covid$mean.temp.lag.2[is.na(covid$mean.temp.lag.2)] <- mean.temp$mean.temp[which(mean.temp$date%in%(covid$date[is.na(covid$mean.temp.lag.2)]-2))]
covid$mean.temp.lag.7[is.na(covid$mean.temp.lag.7)] <- mean.temp$mean.temp[which(mean.temp$date%in%(covid$date[is.na(covid$mean.temp.lag.7)]-7))]
covid$mean.temp.lag.10[is.na(covid$mean.temp.lag.10)] <- mean.temp$mean.temp[which(mean.temp$date%in%(covid$date[is.na(covid$mean.temp.lag.10)]-10))]
covid$mean.temp.lag.28[is.na(covid$mean.temp.lag.28)] <- mean.temp$mean.temp[which(mean.temp$date%in%(covid$date[is.na(covid$mean.temp.lag.28)]-28))]

covid$min.temp.lag.2[is.na(covid$min.temp.lag.2)] <- min.temp$min.temp[which(min.temp$date%in%(covid$date[is.na(covid$min.temp.lag.2)]-2))]
covid$min.temp.lag.7[is.na(covid$min.temp.lag.7)] <- min.temp$min.temp[which(min.temp$date%in%(covid$date[is.na(covid$min.temp.lag.7)]-7))]
covid$min.temp.lag.10[is.na(covid$min.temp.lag.10)] <- min.temp$min.temp[which(min.temp$date%in%(covid$date[is.na(covid$min.temp.lag.10)]-10))]
covid$min.temp.lag.28[is.na(covid$min.temp.lag.28)] <- min.temp$min.temp[which(min.temp$date%in%(covid$date[is.na(covid$min.temp.lag.28)]-28))]

covid$max.temp.lag.2[is.na(covid$max.temp.lag.2)] <- max.temp$max.temp[which(max.temp$date%in%(covid$date[is.na(covid$max.temp.lag.2)]-2))]
covid$max.temp.lag.7[is.na(covid$max.temp.lag.7)] <- max.temp$max.temp[which(max.temp$date%in%(covid$date[is.na(covid$max.temp.lag.7)]-7))]
covid$max.temp.lag.10[is.na(covid$max.temp.lag.10)] <- max.temp$max.temp[which(max.temp$date%in%(covid$date[is.na(covid$max.temp.lag.10)]-10))]
covid$max.temp.lag.28[is.na(covid$max.temp.lag.28)] <- max.temp$max.temp[which(max.temp$date%in%(covid$date[is.na(covid$max.temp.lag.28)]-28))]


## d. Population data ----
# copied from "raw_data/ukpopestimatesmid2020on2021geography.xls"
  # sheet "MYE2 - Persons"

# population estimate for North-West England for mid-2020 = 7,367,456
covid$pop <- rep(7367456, length(covid$date))

## e. Create binary lockdown variables -----

covid$lockdown <- rep(0, length(covid$date))

# second lockdown
covid$lockdown[covid$date > ymd("2020-11-04") & 
                 covid$date < ymd("2020-12-02")] <- 1

# third lockdown 
covid$lockdown[covid$date > ymd("2020-12-25") & 
                 covid$date < ymd("2021-03-30")] <- 1

# 2.Save data as csv file ----

# change variable names for ease of use 

names(covid)[5:10] <- c("cases", "deaths.28", "deaths.60", "vacc.2nd", "vacc.3rd",
                        "reinf")

# re-arrange columns 

names(covid)[c(1,15,2:4, 32, 5:10, 33, 11, 16:19, 12, 20:23, 13, 24:27, 14, 28:31)]

covid <- covid[,c(1,15,2:4, 32, 5:10, 33, 11, 16:19, 12, 20:23, 13, 24:27, 14, 28:31)]

# Include verbose labels 
# doesn't really work well
#library(expss)
#
#covid <- apply_labels(covid,
#             cases = "New cases by specimen date",
#             deaths.28 = "New deaths 28 days after diagnosis, by death date",
#             deaths.60 = "New deaths 60 days after diagnosis, by death date",
#             reinf = "New reinfections by specimen date")
#
#names(covid)
#var_lab(covid$deaths.28)
#

# save as csv 
write.csv(covid, file = "data/covid_data.csv", na = "NA",
          row.names = FALSE)

# test plots 

plot(covid$date, log(covid$newCasesBySpecimenDate), type = "l")
plot(covid$date, log(covid$newDeaths28DaysByDeathDate), type = "l")
plot(covid$date, covid$newPeopleVaccinatedSecondDoseByVaccinationDate,
     type = "l")
plot(covid$date, covid$rain, type = "l")
matplot(covid$date, cbind(covid$min.temp, covid$mean.temp, covid$max.temp),
        type = "l", lty = c("dashed", "solid", "dashed"),
        col = 1)
