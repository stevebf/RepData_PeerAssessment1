# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

First of all, let's read in the data.  Note that we're assuming the CSV file is in our working directory.


```r
setwd("~/Bluefin/R Working Directory/represearchproj1")
activitydata <- read.csv(unz("activity.zip", "activity.csv"))
```

Next, we want to convert the date field, which is stored as text, to be of the R "date" type.


```r
activitydata$date <- as.Date(activitydata$date,"%Y-%m-%d")
```

## What is mean total number of steps taken per day?


Now, use ddply to get the number of steps by day.  Of course, we'll have to use the plyr library for this.


```r
library(plyr)
```

```
## Warning: package 'plyr' was built under R version 3.1.3
```

```r
activitybyday <- ddply(activitydata,~date,summarise,tot=sum(steps,na.rm=TRUE))
```

The following histogram shows the distribution of the number of steps taken in a day.


```r
hist(activitybyday$tot,breaks=20, col="lightblue", main="Histogram of Steps Taken per Day", xlab="Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

Now we're going to get the mean and the median.


```r
mean1 <- mean(activitybyday$tot)
median1 <- median(activitybyday$tot)
```

The mean number of steps taken per day is 9354.2295082.

The median number of steps taken per day is 10395.

## What is the average daily activity pattern?

Again, we use ddply, this time to calculate the average number of steps in each interval. Then we can draw a plot to show how the activity varies, on average, throughout the day.


```r
activitybyinterval <- ddply(activitydata,~interval,summarise,average=mean(steps,na.rm=TRUE))
plot(activitybyinterval$interval, activitybyinterval$average,type = "l", col="green", main="Average steps per interval",xlab="time of day", ylab="number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

To find out the exact interval in which the participant is most active, sort the data frame by number of steps (highest to lowest), and then take the first column in the top row. 


```r
sorted <- activitybyinterval[with(activitybyinterval,order(-activitybyinterval[,"average"])),]
mostactiveinterval <- sorted[1,1]
```

This tells us that the most busy interval, on average, was 835.

## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?