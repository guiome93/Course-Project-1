---
  title: "Course project1"
author: "Guillaume Bianconi"
date: '2022-08-30'
output: html_document
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(chron)
library(lubridate)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(timeDate)
library(RCurl)
```

## Loading and preprocessing the data
This chunk downloads and loads the data, and formats the date.

```{r get data}
wd_path <- getwd()
url <- "http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(url, destfile = 'C5W2Ass-1.zip', method = 'curl')
unzip(zipfile = "C5W2Ass-1.zip")
datRaw <- read.csv("activity.csv")
datRaw$Date <- as.Date(datRaw$date, "%Y-%m-%d") #adds date column in date format
```

## What is mean total number of steps taken per day?
### Note: Missing data is ignored

1. Calculate total number of steps taken per day.
```{r sub steps by day}
total_steps_per_day <- tapply(datRaw$steps, datRaw$Date, sum)
total_steps_per_day
```

2. Make a histogram of the total number of steps taken each day.
```{r histogram}
barplot(total_steps_per_day, las=2, cex.names = .6, col = "red", ylab = "Steps Per Day", 
        main = "Total Steps Per Day")
```

3. Calculate the mean and median of the total number of steps taken per day.
```{r mean and median}
datMean <- tapply(datRaw$steps, datRaw$Date, mean)
datMedian <- tapply(datRaw$steps, datRaw$Date, median)
datMeanMedian <- rbind(datMean, datMedian)
datMeanMedian
```

What is the average daily activity pattern?
  ---------------------------------------------
  1. Make a time series plot of the 5-minute interval (x-axis) and the average number of 
steps taken, averaged across all days (y-axis)
```{r plot average steps by interval}
datAvgByInterval = datRaw %>% group_by(interval) %>%
  summarise(Avg_by_interval = mean(steps, na.rm=TRUE), .groups = 'drop')
plot(datAvgByInterval, type = 'l', xlab = "Interval", ylab = "Average # of Steps", main = "Average Number of Steps by Interval")
```

2. Which 5-minute interval, on average across all the days of the data set, contains
the maximum number of steps?
  ```{R max steps}
#datAvgByInterval[,which.max(datAvgByInterval$Avg_by_interval)]
highestInterval <- datAvgByInterval[which.max(datAvgByInterval$Avg_by_interval), ]
highestInterval2 <- as.data.frame(highestInterval[,1])
highestInterval2
```

Imputing Missing Values
-------------------------
  1. Calculate and report the total number of missing values in the data set.
```{r missing values}
datMissing <- datRaw[is.na(datRaw$steps), ]
nrow(datMissing)
```

2. Devise a strategy for filling in all of the missing values in the dataset.
This program will use mean value of steps across the dataset.

3. Create a new dataset that is equal to the original dataset but with the missing
data filled in.
```{r filling in data to dataset}
datFilled <- datRaw
datFilled$steps[is.na(datFilled$steps)] <- mean(datFilled$steps, na.rm=TRUE)
```

4. Make a histogram of the total number of steps taken each day and calculate and report 
the mean and median total number of steps taken per day. Do these values differ from
the estimates from the first part of the assignment? What is the impact of imputing
missing data on the estimates of the total daily number of steps?
  ```{r create plot and calculate mean and median per day}
total_steps_per_day_filled <- tapply(datFilled$steps, datFilled$Date, sum)
barplot(total_steps_per_day_filled, las=2, cex.names = .6, col = "red", 
        ylab = "Steps Per Day", main = "Total Steps Per Day (NAs filled with mean)")
datMeanFilled <- tapply(datFilled$steps, datFilled$Date, mean)
datMedianFilled <- tapply(datFilled$steps, datFilled$Date, median)
datMeanMedianFilled <- rbind(datMeanFilled, datMedianFilled)
datMeanMedianCombined <- rbind(datMeanMedian, datMeanMedianFilled)
datMeanMedianCombined
```
The above shows the only values that changed with the data filled were the mean and 
median on days which registered an N/A value. Because every day that had an interval
value of N/A contained N/A values for the entire day, there are no days that changed
mean or median values by filling the data. 

Are there differences in activity patterns between weekdays and weekends?
  ---------------------------------------------------------------------------
  
  1. Create a new factor variable in the dataset with two levels - "weekday" and 
"weekend" indicating whether a given date is a weekday or a weekend day. 
```{r adding factor of weekday/weekend to the filled data}
datFilled$weekday <- isWeekday(datFilled$Date)
for (i in 1:nrow(datFilled)){
  if (datFilled[i, 5] == TRUE) 
  { datFilled[i, 6] <- c("Weekday")
  } else {datFilled[i, 6] <- c("Weekend")
  }
}
colnames(datFilled)[6] <- "dayType"
```

2. Make a panel plot containing a time series plot of the 5-minute interval (x-axis)
and the average number of steps taken, averaged across all weekday days or weekend
days (y-axis). 
```{r create panel plot}
datAvgStepsByDayTypeByInterval <- aggregate(steps~interval + dayType, data = datFilled, mean)
ggplot(datAvgStepsByDayTypeByInterval, aes(x = interval, y = steps)) +
  geom_line() + 
  facet_grid(dayType ~ .)
```