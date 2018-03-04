Activity Monitoring Project
===========================

Introduction
------------

It is now possible to collect a large amount of data about personal
movement using activity monitoring devices such as a Fitbit, Nike
Fuelband, or Jawbone Up. These type of devices are part of the
"quantified self" movement - a group of enthusiasts who take
measurements about themselves regularly to improve their health, to find
patterns in their behavior, or because they are tech geeks. But these
data remain under-utilized both because the raw data are hard to obtain
and there is a lack of statistical methods and software for processing
and interpreting the data.

This assignment makes use of data from a personal activity monitoring
device. This device collects data at 5 minute intervals through out the
day. The data consists of two months of data from an anonymous
individual collected during the months of October and November, 2012 and
include the number of steps taken in 5 minute intervals each day.

Dataset
-------

The data for this assignment can be downloaded from the course web site:

Dataset: [Activity monitoring
data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip)
\[52K\]

The variables included in this dataset are:

steps: Number of steps taking in a 5-minute interval (missing values are
coded as NA) date: The date on which the measurement was taken in
YYYY-MM-DD format interval: Identifier for the 5-minute interval in
which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there
are a total of 17,568 observations in this dataset.

Loading and preprocessing the data
----------------------------------

    activity <- read.csv("./activity.csv")

    activity$date <- as.POSIXct(activity$date, "%Y-%m-%d")

    ## Warning in strptime(xx, f <- "%Y-%m-%d %H:%M:%OS", tz = tz): unknown
    ## timezone '%Y-%m-%d'

    ## Warning in as.POSIXct.POSIXlt(x): unknown timezone '%Y-%m-%d'

    ## Warning in strptime(xx, f <- "%Y/%m/%d %H:%M:%OS", tz = tz): unknown
    ## timezone '%Y-%m-%d'

    ## Warning in as.POSIXct.POSIXlt(x): unknown timezone '%Y-%m-%d'

    ## Warning in strptime(xx, f <- "%Y-%m-%d %H:%M", tz = tz): unknown timezone
    ## '%Y-%m-%d'

    ## Warning in as.POSIXct.POSIXlt(x): unknown timezone '%Y-%m-%d'

    ## Warning in strptime(xx, f <- "%Y/%m/%d %H:%M", tz = tz): unknown timezone
    ## '%Y-%m-%d'

    ## Warning in as.POSIXct.POSIXlt(x): unknown timezone '%Y-%m-%d'

    ## Warning in strptime(xx, f <- "%Y-%m-%d", tz = tz): unknown timezone '%Y-%m-
    ## %d'

    ## Warning in as.POSIXct.POSIXlt(x): unknown timezone '%Y-%m-%d'

    ## Warning in strptime(x, f, tz = tz): unknown timezone '%Y-%m-%d'

    ## Warning in as.POSIXct.POSIXlt(as.POSIXlt(x, tz, ...), tz, ...): unknown
    ## timezone '%Y-%m-%d'

    summary(activity)

    ## Warning in as.POSIXlt.POSIXct(x, tz): unknown timezone '%Y-%m-%d'

    ##      steps             date               interval     
    ##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
    ##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
    ##  Median :  0.00   Median :2012-10-31   Median :1177.5  
    ##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
    ##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
    ##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
    ##  NA's   :2304

What is mean total number of steps taken per day?
-------------------------------------------------

### Sum steps by day

    steps_by_day <- aggregate(steps ~ date, activity , sum)

    ## Warning in as.POSIXlt.POSIXct(x, tz): unknown timezone '%Y-%m-%d'

    ## Warning in as.POSIXlt.POSIXct(x, tz): unknown timezone '%Y-%m-%d'

### create Histogram of the total number of steps taken each day

    hist(steps_by_day$steps, main = paste("Total Steps Each Day"),  xlab="Number of Steps")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-3-1.png)

### mean and median of the total number of steps taken per day

    steps_mean <- mean(steps_by_day$steps)

    steps_median <- median(steps_by_day$steps)

What is the average daily activity pattern?
-------------------------------------------

### Create Plot of 5 minute interval and the number of avg number of steps taken

    steps_by_interval <- aggregate(steps ~ interval, activity, mean)

    plot(steps_by_interval$interval,steps_by_interval$steps, type="l", 
    xlab="5 Minute Interval", ylab="Average Number of Steps", main="Average Number of Steps per Day by Interval")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-5-1.png)

### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

    steps_by_interval[which.max(steps_by_interval$steps), ]$interval

    ## [1] 835

Imputing missing values
-----------------------

### Calculate and report the total number of missing values in the dataset

    sum(is.na(activity))

    ## [1] 2304

### Devise a strategy for filling in all of the missing values in the dataset

#### I will replace all NA's with the average of its interval . eg : If interval is 20 , then will take average of steps in interval 20 for all days and replace .

### Create a new dataset that is equal to the original dataset but with the missing data filled in

    activity_imputed <- transform(activity, steps = ifelse(is.na(activity$steps), 
          yes = steps_by_interval$steps[match(activity$interval, steps_by_interval$interval)], no = activity$steps))
    steps_imputed <- aggregate(steps ~ date, activity_imputed, sum)

    ## Warning in as.POSIXlt.POSIXct(x, tz): unknown timezone '%Y-%m-%d'

    ## Warning in as.POSIXlt.POSIXct(x, tz): unknown timezone '%Y-%m-%d'

    names(steps_imputed) <- c("date", "steps")

### Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

    hist(steps_imputed$steps , xlab = "Total steps per day", main = "Total number of steps taken each day" )

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-9-1.png)

### mean and median of the total number of steps taken per day

    mean_imputed <- mean(steps_imputed$steps)
    median_imputed <- median(steps_imputed$steps)

### Difference between Old and imputed values

    mean_difference <- mean_imputed - steps_mean
    med_difference <- median_imputed - steps_median

### Difference in the sum of the steps

    sum_diff_OldAndImputed <- sum(steps_imputed$steps) - sum(steps_by_day$steps)

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

### Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

    activity$week_by_day <- factor(format(steps_imputed$date, "%A"))

    ## Warning in as.POSIXlt.POSIXct(x, tz): unknown timezone '%Y-%m-%d'

    levels(activity$week_by_day) <- list(weekday = c("Monday", "Tuesday",
                                                  "Wednesday", "Thursday",
                                                  "Friday"), weekend =
                                              c("Saturday", "Sunday"))

### Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

    par(mfrow = c(2, 1))

    with(activity[activity$week_by_day == "weekend",], plot(aggregate(steps ~ interval, FUN = mean), type = "l", main = "Weekends"))

    with(activity[activity$week_by_day == "weekday",], plot(aggregate(steps ~ interval, FUN = mean), type = "l", main = "Weekdays"))

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-14-1.png)
