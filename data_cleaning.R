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

covid$newPeopleVaccinatedSecondDoseByVaccinationDate[is.na(covid$newPeopleVaccinatedSecondDoseByVaccinationDate)] <- 0 
covid$newPeopleVaccinatedThirdInjectionByVaccinationDate[is.na(covid$newPeopleVaccinatedThirdInjectionByVaccinationDate)] <- 0


# merge covid data with environmental data 

covid <- merge(covid, rain[,c(4,5)], by = "date")
covid <- merge(covid, mean.temp[,c(4,5)], by = "date")
covid <- merge(covid, min.temp[,c(4,5)], by = "date")
covid <- merge(covid, max.temp[,c(4,5)], by = "date")

## d. Population data ----
# copied from "raw_data/ukpopestimatesmid2020on2021geography.xls"
  # sheet "MYE2 - Persons"

# population estimate for North-West England for mid-2020
covid$pop <- rep(7367456, length(covid$date))

# 2.Save data as csv file ----

# change variable names for ease of use 

names(covid)[5:10] <- c("cases", "deaths.28", "deaths.60", "vacc.2nd", "vacc.3rd",
                        "reinf")

# Include verbose labels 
library(expss)

covid <- apply_labels(covid,
             cases = "New cases by specimen date",
             deaths.28 = "New deaths 28 days after diagnosis, by death date",
             deaths.60 = "New deaths 60 days after diagnosis, by death date",
             reinf = "New reinfections by specimen date")

names(covid)
var_lab(covid$deaths.28)

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
