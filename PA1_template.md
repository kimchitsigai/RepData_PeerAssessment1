---
title: "Reproducible Research: Peer Assessment 1"
author: "kimchitsigai"
date: "13 novembre 2015"
output: html_document
---


## Loading and preprocessing the data


```r
activity <- read.csv("activity.csv")
activity$date <- as.Date(activity$date)
## Add a column with the time interval index
for (i in 1:nrow(activity)) {
      activity[i,"index"] <- floor(activity[i,3]/100)*12 + (activity[i,3]%%100)/5 + 1
}
summary(activity)
```

```
##      steps             date               interval          index       
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0   Min.   :  1.00  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8   1st Qu.: 72.75  
##  Median :  0.00   Median :2012-10-31   Median :1177.5   Median :144.50  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5   Mean   :144.50  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2   3rd Qu.:216.25  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0   Max.   :288.00  
##  NA's   :2304
```


## What is mean total number of steps taken per day?

The total number of steps taken per day is :


```r
stepsperday <- aggregate(activity[, 1], list(date=activity$date), sum)
```

The histogram below gives the frequencies of the number of steps taken per day.


```r
hist(stepsperday$x, breaks=20, main="Number of steps per day", xlab="Steps per day")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

The mean of the total number of steps taken per day is :


```r
mean(na.omit(stepsperday$x))
```

```
## [1] 10766.19
```

The median of the total number of steps taken per day is :


```r
median(na.omit(stepsperday$x))
```

```
## [1] 10765
```


## What is the average daily activity pattern?

The average daily pattern, per 5 minute interval, is :


```r
stepsperinterval <- aggregate(na.omit(activity[, 1]), 
                              list(interval=activity$interval[!is.na(activity$steps)]), mean)
for (i in 1:nrow(stepsperinterval)) {
      stepsperinterval[,"index"] <- seq(1:288)
}
plot(stepsperinterval$index, stepsperinterval$x, 
     main="Average daily pattern per 5mn interval",
     xlab="Interval",
     ylab="Steps per interval",
     type="l",
     lty="solid",
     col="blue")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 

The interval which contains the maximum number of steps is :


```r
stepsperinterval$interval[stepsperinterval$x == max(stepsperinterval$x)]
```

```
## [1] 835
```


## Imputing missing values

The total number of missing values is :


```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

The strategy chosen for filling in the missing values is to use the mean number of steps for that interval.


```r
for (i in 1:nrow(activity)) {
        if(is.na(activity[i,1])) {
              activity[i,1] <- stepsperinterval[activity[i,4], 2]
        }
}
```

The corrected total number of steps taken per day is :


```r
stepsperday <- aggregate(activity[, 1], list(date=activity$date), sum)
```

The histogram below gives the frequencies of the corrected number of steps taken per day.


```r
hist(stepsperday$x, breaks=20, main="Corrected number of steps per day", xlab="Steps per day")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 

The mean of the corrected total number of steps taken per day is :


```r
mean(na.omit(stepsperday$x))
```

```
## [1] 10766.19
```

The median of the corrected total number of steps taken per day is :


```r
median(na.omit(stepsperday$x))
```

```
## [1] 10766.19
```


## Are there differences in activity patterns between weekdays and weekends?


```r
activity$day <- weekdays(activity$date)
for (i in 1:nrow(activity)) {
      if(activity[i,"day"] %in% c("samedi", "dimanche"))
              activity[i,"daytype"] <- "weekend"
      else
              activity[i,"daytype"] <- "weekday"
}
weekday_activity <- subset(activity, activity$daytype == "weekday")
weekend_activity <- subset(activity, activity$daytype == "weekend")

weekday_steps <- aggregate(weekday_activity[, 1], 
                              list(interval=weekday_activity$interval), mean)
weekend_steps <- aggregate(weekend_activity[, 1], 
                              list(interval=weekend_activity$interval), mean)
for (i in 1:nrow(weekday_steps)) 
      weekday_steps[,"index"] <- seq(1:288)
for (i in 1:nrow(weekend_steps)) 
      weekend_steps[,"index"] <- seq(1:288)

par(mfrow=c(2,1), pin=c(5,5), mai=c(1,0.5,0.5,0))
plot(weekday_steps$index, weekday_steps$x, 
     main="Average weekday daily pattern per 5mn interval",
     xlab="Interval",
     ylab="Steps",
     type="l",
     lty="solid",
     col="blue")
plot(weekend_steps$index, weekend_steps$x, 
     main="Average weekend daily pattern per 5mn interval",
     xlab="Interval",
     ylab="Steps",
     type="l",
     lty="solid",
     col="red")
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14-1.png) 




