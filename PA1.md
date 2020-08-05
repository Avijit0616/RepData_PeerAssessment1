---
title: 'Reproducible Research: Week 2 Assignment'
author: "Avijit"
date: "14^th^ July 2020"
output: 
  html_document: 
    keep_md: yes
---


## Loading and processing data

```r
if(!file.exists("./activity.csv")){
download.file(url = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
              ,destfile = "./activity_data.zip")
unzip(zipfile = "./activity_data.zip")
}
d <- fread("./activity.csv")
```

## Number of steps taken each day


```r
bydate <- d %>%
    select(steps,date) %>%
    group_by(date) %>%
    summarise(total_steps = sum(steps)) %>%
    na.omit()

hist(bydate$total_steps,
     xlab = "Total daily Steps",main="Histogram of Total Steps by day",
     breaks = 20)
```

![](PA1_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

## Mean and median of the total number of steps taken per day

```r
mean(bydate$total_steps)
```

```
## [1] 10766.19
```

```r
median(bydate$total_steps)
```

```
## [1] 10765
```

## Time series plot of the 5-minute interval and the average number of steps taken, averaged across all days

```r
byint <- d %>%
    select(steps, interval) %>%
    na.omit() %>%
    group_by(interval) %>%
    summarise(average_steps = mean(steps))

ggplot(byint, aes(x = interval, y = average_steps)) + geom_line()
```

![](PA1_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

## 5-minute interval, on average across all the days, that contains the maximum number of steps

```r
byint[which.max(byint$average_steps),]
```

```
## # A tibble: 1 x 2
##   interval average_steps
##      <int>         <dbl>
## 1      835          206.
```

## Number of missing values in the dataset

```r
length(which(is.na(d$steps)))
```

```
## [1] 2304
```

## Imputing missing values and replacing them with the mean for that 5 -minute interval

```r
replacewithmean <- function(x){
    replace(x,is.na(x), mean(x, na.rm = T))
}
byintmean <- d %>%
    group_by(interval) %>%
    mutate(steps = replacewithmean(steps))

bydate_impute <- byintmean %>%
    group_by(date) %>%
    summarise(total_steps = sum(steps))

qplot(total_steps, data = bydate_impute, binwidth = 500,
      xlab='Steps per day', ylab='Frequency',
      main = 'Histogram of the total number of steps taken each day')
```

![](PA1_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

## Mean and median of imputed and original data

```r
summary(bydate_impute)
```

```
##      date            total_steps   
##  Length:61          Min.   :   41  
##  Class :character   1st Qu.: 9819  
##  Mode  :character   Median :10766  
##                     Mean   :10766  
##                     3rd Qu.:12811  
##                     Max.   :21194
```

```r
summary(bydate)
```

```
##      date            total_steps   
##  Length:53          Min.   :   41  
##  Class :character   1st Qu.: 8841  
##  Mode  :character   Median :10765  
##                     Mean   :10766  
##                     3rd Qu.:13294  
##                     Max.   :21194
```

## Activity patterns on weekdays and weekends

```r
byintmean$date <- as.Date(byintmean$date)
byintmean$day <- weekdays(byintmean$date)
byintmean$weekday <- with(byintmean,
                          ifelse(day == "Saturday"|day == "Sunday","Weekend", "Weekday"))

byweekend <- byintmean %>%
    group_by(weekday,interval) %>%
    summarise(avg = mean(steps))
qplot(interval, avg, data = byweekend, facets = weekday~., color = weekday,
      geom = "line")
```

![](PA1_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

