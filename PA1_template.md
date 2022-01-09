---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
```

```r
library(ggplot2)
```

## Loading and preprocessing the data

Below codes will load the activity data into R and makes some structural changes to the dataset.


```r
data <- read.csv("activity.csv", header = T)
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
data$date <- as.Date(data$date)
tail(data$date, n =10, na.rm = T)
```

```
##  [1] "2012-11-30" "2012-11-30" "2012-11-30" "2012-11-30" "2012-11-30"
##  [6] "2012-11-30" "2012-11-30" "2012-11-30" "2012-11-30" "2012-11-30"
```



## What is mean total number of steps taken per day?


```r
steps_day <- data.frame(steps = tapply(data$steps, data$date, sum, na.rm = T))
steps_day$date<-rownames(steps_day)
rownames(steps_day) <- NULL
steps_day$date <- ymd(steps_day$date)
str(steps_day)
```

```
## 'data.frame':	61 obs. of  2 variables:
##  $ steps: int  0 126 11352 12116 13294 15420 11015 0 12811 9900 ...
##  $ date : Date, format: "2012-10-01" "2012-10-02" ...
```

```r
mean(steps_day$steps)
```

```
## [1] 9354.23
```

```r
median(steps_day$steps)
```

```
## [1] 10395
```

```r
ggplot(data = steps_day, aes(x = date, y = steps), ) +
        geom_histogram(stat = "identity", color = "red")
```

```
## Warning: Ignoring unknown parameters: binwidth, bins, pad
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

It turns out that the mean number of steps taken each day is 9354.23. The median steps per day is 10395. 

## What is the average daily activity pattern?


```r
steps_interval <- data.frame(steps = tapply(data$steps,data$interval,
                                            sum,na.rm = T))
steps_interval$interval <- rownames(steps_interval)
rownames(steps_interval) <- NULL

ggplot(data = steps_interval, aes(x = interval, y = steps)) + 
        geom_histogram(stat = "identity")
```

```
## Warning: Ignoring unknown parameters: binwidth, bins, pad
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
steps_interval %>%
        filter(steps == max(steps))
```

```
##   steps interval
## 1 10927      835
```

The plot is shown above. The interval '835' contains the maximum number of steps.


## Imputing missing values


```r
sum(is.na(data))
```

```
## [1] 2304
```

```r
data[is.na(data)] = 0

steps_day2 <- data.frame(steps = tapply(data$steps, data$date, sum, na.rm = T))
steps_day2$date <- rownames(steps_day2)
rownames(steps_day2) <- NULL
steps_day2$date <- ymd(steps_day2$date)  

ggplot(data = steps_day2, aes(x = date, y = steps)) + 
        geom_histogram(stat = "identity")
```

```
## Warning: Ignoring unknown parameters: binwidth, bins, pad
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
mean(steps_day2$steps)
```

```
## [1] 9354.23
```

```r
median(steps_day2$steps)
```

```
## [1] 10395
```

The data refers to the number of steps in each interval. To be on the conservative side, it is better to replace missing values with NA, in order to get the worst possible results. 

There seems to be no difference in our mean and median after imputing missing values because our initial analysis was already conservative. 


## Are there differences in activity patterns between weekdays and weekends?


```r
data$datetype <- sapply(data$date, function(x) {
        if (weekdays(x) == "Saturday" | weekdays(x) =="Sunday") 
        {y <- "Weekend"} else 
        {y <- "Weekday"}
        y
})

table(data$datetype)
```

```
## 
## Weekday Weekend 
##   12960    4608
```

```r
activity_by_date <- aggregate(steps~interval + datetype, data, mean, na.rm = TRUE)
plot<- ggplot(activity_by_date, aes(x = interval , y = steps, color = datetype)) +
        geom_line() +
        labs(title = "Average daily steps by type of date", x = "Interval", y = "Average number of steps") +
        facet_wrap(~datetype, ncol = 1, nrow=2)
print(plot)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

