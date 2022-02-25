# 1.Load data ====
library(reshape2)
library(stringr)

## Temperature data ------

# Create function to import temperature data

clean.temp.data <- function(data, temp.name){
  
  require(reshape2)
  require(stringr)
  
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
  
  for (i in 1:length(temp[[temp.name]])) {
    
    if (nchar(abs(temp[[temp.name]][i]))==1 | is.na(temp[[temp.name]][i])==TRUE) {
      
      temp[[temp.name]][i] <- temp[[temp.name]][i]
      
    } else { 
      
      temp[[temp.name]][i] <- as.numeric(paste0(str_sub(temp[[temp.name]][i],
                                                          1, (nchar(temp[[temp.name]][i])-1)),
                                                  ".",
                                                  str_sub(temp[[temp.name]][i], nchar(temp[[temp.name]][i]),
                                                          nchar(temp[[temp.name]][i]))))
      
    }
    
  }
  
  # order data frame by Yeah, Month and then Day 
  
  temp <- temp[order(temp$Year,
                     temp$Month,
                     temp$Day),]
  
  
  return(temp)
  
}

mean.temp <- clean.temp.data(data = "raw_data/daily_HadCET_1772_2022.txt",
                             temp.name = "mean.temp")

min.temp <- clean.temp.data(data = "raw_data/daily_min_HadCET_1878_2022.txt",
                            temp.name = "min.temp")

max.temp <- clean.temp.data(data = "raw_data/daily_max_HadCET_1878_2022.txt",
                            temp.name = "max.temp")

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

