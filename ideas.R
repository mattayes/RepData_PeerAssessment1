## Activity Monitoring Data Exploration

## Find the activity.zip file here:
## https://github.com/mattayes/RepData_PeerAssessment1

## Packages
library(tidyr)
library(dplyr)
library(lubridate)

## Unzip and read the data
if(!file.exists("./activity.csv")) {unzip("./activity.zip")}
activity <- tbl_df(read.csv("./activity.csv",
                            stringsAsFactors = FALSE,
                            colClasses = c("integer", "character", "integer")
                            )
                   )


## Combine date and interval into dateTime object
## This approach is more "pure" but is a pain in the ass
activity <- activity %>%
    mutate(interval = formatC(interval, width = 4, format = "d", flag = "0"),
           interval = paste(interval, "00", sep = ""),
           dateTime = paste(date, interval),
           dateTime = ymd_hms(dateTime)
    ) %>%
    select(dateTime, steps)

## Above method is too hard. Use formatC for prettier plots later
activity <- activity %>%
    mutate(interval = formatC(interval, width = 4, format = "d", flag = "0"))



## Make interval more readable (useless)
test <- activity
test <- mutate(test, 
               interval = as.numeric(interval),
               int_char = as.character(interval)
               )
with(test,
     for(i in 1:length(unique(interval))) {
        if(interval[i] < 10){
            int_char[i] <- paste("00000", int_char[i], sep = "")
        }
        else if(integer[i] >= 10 & integer[i] <= 55){
            int_char[i] <- paste("0000", int_char[i], sep = "")
        }
        else if(interval[i] >= 100 & interval[i] <=995){
            int_char[i] <- paste("000", int_char[i], sep = "")
        }
        else {
            int_char[i] <- int_char[i]
        }
     }
)

## Random Stuff
activity
tail(activity)
2355 / 15
max(interval)
max(activity$interval)
filter(activity, interval == 2355)
glimpse(activity)
24 * 5
(60 / 5) * 24
2355 / 5
60 / 5
