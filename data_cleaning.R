# 1.Load data ====
library(reshape2)

## Mean Temperature data ------

temp <- read.fwf("raw_data/daily_HadCET_1772_2022.txt",
                 widths = c(6, rep(5, 13)),
                 header = FALSE)

# rename columns
names(temp) <- c("Year", "Day", "Jan", "Feb", "Mar", "Apr",
                                "May", "Jun", "Jul", "Aug",
                                "Sep", "Oct", "Nov", "Dec")

# change -999 to NA
temp[temp == -999] <- NA

# wide to long
temp <- melt(temp, id = c("Year", "Day"))
names(temp)[c(3,4)] <- c("Month", "mean.temp")

# change month to factor

temp$Month <- factor(temp$Month,
                     levels = c("Jan", "Feb", "Mar", "Apr",
                                "May", "Jun", "Jul", "Aug",
                                "Sep", "Oct", "Nov", "Dec"),
                     ordered = TRUE)

# order data frame by Yeah, Month and then Day 

temp <- temp[order(temp$Year,
                   temp$Month,
                   temp$Day),]

## Rainfall data -----

rain <- read.fwf("raw_data/daily_rainfall_NWEP_HadUKP.txt", 
                 widths = c(5, 5, rep(7, 31)),
                 header = FALSE)

# rename columns
names(rain) <- c("Year", "Month", seq(1, 31, by = 1))

# change -99.99 to NA

rain[rain==-99.99] <- NA

## COVID data ----

covid <- read.csv("raw_data/region_2022-02-07.csv")

## Population data ----
# copied from "raw_data/ukpopestimatesmid2020on2021geography.xls"
  # sheet "MYE2 - Persons"

# population estimate for North-West England for mid-2020
pop <- 7367456

