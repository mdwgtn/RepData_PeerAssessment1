---
title: "Reproducible Research: Peer Assessment 1"
author: "Mike Davidson"
date: "8 August 2015"
output: html_document
keep_md: true
---

**Loading and preprocessing the data**

Show any code that is needed to

1. Load the data (i.e. read.csv())

1. Process/transform the data (if necessary) into a format suitable for your analysis


```r
activity <- read.csv("activity.csv")
```

**What is mean total number of steps taken per day?**

For this part of the assignment, you can ignore the missing values in the dataset.

1. Calculate the total number of steps taken per day


```r
library(plyr)
stepsPerDay <- ddply(activity, .(date), summarize,steps = sum(steps,na.rm=TRUE))
```

2. Make a histogram of the total number of steps taken each day


```r
hist(stepsPerDay$steps, breaks=30, xlab="Steps per day",main="Steps per day")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

3. Calculate and report the mean and median of the total number of steps taken per day


```r
stepsMean = mean(stepsPerDay$steps)
stepsMedian = median(stepsPerDay$steps)
paste("Mean steps per day: ", mean(stepsPerDay$steps))
```

```
## [1] "Mean steps per day:  9354.22950819672"
```

```r
paste("Median steps per day: ", median(stepsPerDay$steps))
```

```
## [1] "Median steps per day:  10395"
```

**What is the average daily activity pattern?**

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
stepsPerInterval <- ddply(activity, .(interval), summarise, mean(steps,na.rm=TRUE))
colnames(stepsPerInterval) <- c("interval","steps")
plot(stepsPerInterval$interval,stepsPerInterval$steps,type="l",xlab="Interval",ylab="Steps",main="Average steps taken per 5-minute interval")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
stepsPerInterval <- stepsPerInterval[stepsPerInterval$steps == max(stepsPerInterval$steps),]
paste ("Interval with maximum average number of steps: ", stepsPerInterval$interval)
```

```
## [1] "Interval with maximum average number of steps:  835"
```

**Imputing missing values**

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
naRows <- sum(is.na(activity$steps))
paste("Count of rows with a missing value: ", naRows)
```

```
## [1] "Count of rows with a missing value:  2304"
```

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

1. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
# For a missing interval, use the average for that interval across the whole data set.
activity$steps <- ifelse(is.na(activity$steps), 
       stepsPerInterval$steps[match(activity$interval,stepsPerInterval$interval)], activity$steps)
```

2. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
interpolatedStepsPerDay <- ddply(activity, .(date), summarize,steps = sum(steps,na.rm=TRUE))
hist(interpolatedStepsPerDay$steps, breaks=30,xlab="Steps per day",main="Steps per day, interpolating missing values")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png) 

```r
stepsMean = mean(interpolatedStepsPerDay$steps)
stepsMedian = median(interpolatedStepsPerDay$steps)
paste("Mean steps per day: ", mean(interpolatedStepsPerDay$steps))
```

```
## [1] "Mean steps per day:  9381.26817197649"
```

```r
if (mean(interpolatedStepsPerDay$steps) != mean(stepsPerDay$steps))
  paste("  This differs from the pre-interpolation mean of ", mean(stepsPerDay$steps))
```

```
## [1] "  This differs from the pre-interpolation mean of  9354.22950819672"
```

```r
paste("Median steps per day: ", median(interpolatedStepsPerDay$steps))
```

```
## [1] "Median steps per day:  10395"
```

```r
if (median(interpolatedStepsPerDay$steps) != median(stepsPerDay$steps))
  paste("  This differs from the pre-interpolation median of ", median(stepsPerDay$steps))
```

**Are there differences in activity patterns between weekdays and weekends?**

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
library(lubridate)
activity <- mutate(activity, daytype = ifelse(weekdays(ymd(activity$date), abbreviate = TRUE) %in% c("Sun", "Sat"), "weekend","weekday")) 
activity$daytype <- as.factor(activity$daytype)
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
library(lattice)
weekdays <- activity[activity$daytype == "weekday",]
weekday_intervals <- ddply(weekdays, .(interval,daytype), summarise, mean(steps,na.rm=TRUE))
colnames(weekday_intervals) <- c("interval","daytype","steps")

weekend <- activity[activity$daytype == "weekend",]
weekend_intervals <- ddply(weekend, .(interval,daytype), summarise, mean(steps,na.rm=TRUE))
colnames(weekend_intervals) <- c("interval","daytype","steps")

intervals <- rbind(weekday_intervals,weekend_intervals)
with(intervals,xyplot(steps ~ interval | daytype, type="l",layout=c(1,2)))
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 
