Research Project 1 (aka mini-project)
================

# Data Sources and cleaning

## Sources

These were the data sources used: - COVID Data: [UK Coronavirus
Dashboard](https://coronavirus.data.gov.uk/ "UK COVID Dashboard") -
Population: [ONS Population
Estimates](https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland "UK Population Estimates")
- Lockdown timeline: - [Institute for
Government](https://www.instituteforgovernment.org.uk/sites/default/files/timeline-coronavirus-lockdown-december-2021.pdf)
- [House of Commons
library](https://researchbriefings.files.parliament.uk/documents/CBP-9068/CBP-9068.pdf)
- Temperature: Met Office [Hadley Centre Central England Temperature
Data](https://www.metoffice.gov.uk/hadobs/hadcet/data/download.html "HadCET")
- Rainfall: Met Office Hadley Centre observations datasets:
[HadUKP](https://www.metoffice.gov.uk/hadobs/hadukp/ "HadUKP")

## Data Cleaning

###Temperature Data

Data was saved as a `.txt` file. It is stored as follows: - The first
column corresponding to the year - The second column corresponding to
the day - Each column thereafter corresponding to the month (12 columns,
one for each month).

Each cell represents daily values, expressed in tenths of a °C.

To import data, I created the following function

``` r
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
```

I used this function to import minimum, maximum and mean daily
temperatures into a data frame in R.
