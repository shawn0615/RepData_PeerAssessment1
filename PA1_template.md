---
title: 'Reproducible Research: Peer Assessment 1'
output:
  html_document:
    keep_md: true
 
---


## Setting echo equal to TRUE.

opts_chunk$set(echo = TRUE, results = 'hold')

## Loading and preprocessing the data
##```{r}

setwd("~/Downloads/Reproducible_Research")
library(knitr)
knit("PA1_template.Rmd")
browseURL(".PA1_template.html")

require("ggplot2") 
require("lattice") 


data <- read.csv("activity.csv")
without_na <- data[complete.cases(data),]

## What is mean total number of steps taken per day?

##1. Make a histogram of the total number of steps taken each day

total_steps_per_day <- aggregate(steps ~ date, data, sum)

hist(total_steps_per_day$steps, main = paste("Total Steps Per Day"), col="gray", xlab="Number of Steps")

##2. Calculate and report the mean and median total number of steps taken per day

mean(total_steps_per_day$steps)

median(total_steps_per_day$steps)

##3. What is the average daily activity pattern?

avg <- aggregate(steps ~ interval, without_na, mean)

names(avg)[2] <- "mean_steps"

## 4.  Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.  

steps_by_int<-aggregate(steps ~ interval, data, mean)

with(steps_by_int,plot(interval,steps,type="l"))

max(avg)

subset(steps_by_int,steps==max(steps))$interval

## Do these values differ from the estimates from the first part of the assignment?

## Yes, these values differ from the estimates from the first part of the assignment

## What is the impact of imputing missing data on the estimates of the total daily number of steps?


## Imputing missing values

##1. Calculate and report the total number of missing values in the dataset  (i.e. the total number of rows with \color{red}{\verb|NA|}NAs)

nrow(data[is.na(data$steps),])

##2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

##3.  Create a new dataset that is equal to the original dataset but with the missing data filled in.

newdata <- merge(data, avg, by = 'interval', all.y = F)

## merge NA values with averages rounding up for integers
newdata$steps[is.na(newdata$steps)] <- as.integer(
        round(newdata$mean_steps[is.na(newdata$steps)]))

keeps <- names(data)
newactivity <- newdata[keeps]


## (total number of (steps taken per day))
newtotal <- aggregate(steps ~ date, newactivity, sum)

## add descriptive variable names
names(newtotal)[2] <- "sum_steps"

## plot histogram
hist(
        newtotal$sum_steps,
        col = "gray",
        main = "Histogram of Total Number of Steps Taken Each Day",
        xlab = "Total Steps Per Day",
        breaks = 20
)

## mean
mean(newtotal$sum_steps)

## median
median(newtotal$sum_steps)


##Are there differences in activity patterns between weekdays and weekends?

  ## new data frame
newdataactivity <- newdata

##1.  Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
## set up logical/test vector

weekend <- weekdays(as.Date(newdataactivity$date)) %in% c("Saturday", "Sunday")

newdataactivity$daytype <- "weekday"

## replace "weekday" with "weekend" where day == Sat/Sun

newdataactivity$daytype[weekend == TRUE] <- "weekend"

## convert new character column to factor
newdataactivity$daytype <- as.factor(newdataactivity$daytype)

##2.  Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

## the average number of steps taken, averaged across all days for each 5-minute
newinterval <- aggregate(steps ~ interval + daytype, newdataactivity, mean)

## add descriptive variable names
names(newinterval)[3] <- "mean_steps"



xyplot(
        mean_steps ~ interval | daytype,
        newinterval,
        type = "l",
        layout = c(1,2),
        main = "Time Series Plot",
        xlab = "5-Minute Interval",
        ylab = "Average Number of Steps Taken"
)


##```
