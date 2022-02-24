# 1.Load data ====

## Temperature data ------

temp <- read.fwf("raw_data/daily_HadCET_1772_2022.txt",
                 widths = c(6, rep(5, 13)),
                 header = FALSE)

# rename columns
names(temp) <- c("Year", "Day", "Jan", "Feb", "Mar", "Apr",
                                "May", "Jun", "Jul", "Aug",
                                "Sep", "Oct", "Nov", "Dec")

# change -999 to NA
temp[temp == -999] <- NA

## Rainfall data -----

rain <- read.fwf("raw_data/daily_rainfall_NWEP_HadUKP.txt", 
                 widths = c(5, 5, rep(7, 31)),
                 header = FALSE)

# rename columns
names(rain) <- c("Year", "Month", seq(1, 31, by = 1))

# change -99.99 to NA

rain[rain==-99.99] <- NA
