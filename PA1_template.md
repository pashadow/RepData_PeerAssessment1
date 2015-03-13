# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
library(plyr)
activity <- read.csv("data/activity.csv")
head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

## What is mean total number of steps taken per day?

1. Make a histogram of the total number of steps taken each day


```r
# ignore the missing values in the dataset
tidyData <- activity[!is.na(activity$steps), ]
activityPerDay <- ddply(tidyData, .(date), summarize, steps=sum(steps))
hist(activityPerDay$steps, breaks = "FD", col = "blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

2. Calculate and report the mean and median total number of steps taken per day


```r
mean(activityPerDay$steps, na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
median(activityPerDay$steps, na.rm = TRUE)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
activityPerInterval <- ddply(activity, .(interval), summarize, averagesteps=round(mean(steps, na.rm = TRUE)))
with(activityPerInterval, plot(interval, averagesteps, type = "l"))
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
activityPerInterval[which.max(activityPerInterval$averagesteps), ]
```

```
##     interval averagesteps
## 104      835          206
```

## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
sum(is.na(activity))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
tidyData3 <- within(merge(activity, activityPerInterval), {
    steps = ifelse(is.na(steps), averagesteps, steps)
})
tidyData3 <- tidyData3[, 1:3]
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
activityPerDay2 <- ddply(tidyData3, .(date), summarize, steps=sum(steps))
hist(activityPerDay2$steps, breaks = "FD", col = "red")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png) 


```r
mean(activityPerDay2$steps, na.rm = TRUE)
```

```
## [1] 10765.64
```

```r
median(activityPerDay2$steps, na.rm = TRUE)
```

```
## [1] 10762
```

## Are there differences in activity patterns between weekdays and weekends?


```r
activity$weekday <- weekdays(as.Date(activity$date))
weekend <- c("Saturday", "Sunday")
activity$daytype <- factor(ifelse(activity$weekday %in% weekend, "weekend", "weekday"))

par(mfrow = c(2,1))

weekdayData <- activity[activity$daytype == "weekday", ]
weekdayDataPerInterval <- ddply(weekdayData, .(interval), summarize, averagesteps=round(mean(steps, na.rm = TRUE)))
with(weekdayDataPerInterval, plot(interval, averagesteps, type = "l", main = "weekday"))

weekendData <- activity[activity$daytype == "weekend", ]
weekendDataPerInterval <- ddply(weekendData, .(interval), summarize, averagesteps=round(mean(steps, na.rm = TRUE)))
with(weekendDataPerInterval, plot(interval, averagesteps, type = "l", main = "weekend"))
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png) 
