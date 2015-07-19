---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

Reproducible Research Peer Assessed, Assignment 1
========================================================
## Loading and preprocessing the data

 1. Load the data
```{r}
activity<-read.csv("activity/activity.csv")
head(activity)
```

 2. Process/transform the data (if necessary) into a format suitable for your analysis
 remove NA
```{r}
activityComplete<-activity[complete.cases(activity),]
head(activityComplete)
```

## What is mean total number of steps taken per day?

 1. Calculate the total number of steps taken per day
```{r}
library(plyr); library(dplyr)
totalsteps<-ddply(activityComplete,"date",summarise,steps=sum(steps))
head(totalsteps)
```

 2. Make a histogram of the total number of steps taken each day
```{r}
hist(totalsteps$steps,breaks=20,xlab="Steps per Day",main="Frequency of Steps Taken",col="green")
```

 3. Calculate and report the mean and median of the total number of steps taken per day
```{r}
avg<-mean(totalsteps$steps)
med<-median(totalsteps$steps)
avgmed<-data.frame(c("Mean","Median"),c(avg,med))
names(avgmed)<-c("Measure","Number")
print(avgmed)
```

## What is the average daily activity pattern?

 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average 
 number of steps taken, averaged across all days (y-axis)
```{r}
library(plyr); library(dplyr)
timeseries<-ddply(activityComplete,"interval",summarise,steps=mean(steps))
time1<-strptime(timeseries$interval[1:12],"%M")
time2<-strptime(paste("0",as.character(timeseries$interval[13:120]),sep=""),"%H%M")
time3<-strptime(timeseries$interval[121:288],"%H%M")
timecol<-c(time1,time2,time3)
timeseries$timeint<-timecol
with(timeseries,plot(timeint,steps,type="l"))
```

 2. Which 5-minute interval, on average across all the days in the dataset, 
 contains the maximum number of steps?
```{r}
strftime(timeseries[which.max(timeseries$steps),3],format="%H:%M")
```

## Imputing missing values

 1. Calculate and report the total number of missing values in the dataset 
 (i.e. the total number of rows with NAs)
```{r}
activityNA<-activity[is.na(activity),]
length(activityNA[,1])
```

 2. Devise a strategy for filling in all of the missing values in the dataset.
 The strategy does not need to be sophisticated. For example, you could use the 
 mean/median for that day, or the mean for that 5-minute interval, etc.
        
```{r}
        ## Step 1 add a column from timeseries joined on interval to get the average for that interval
timeseriesM<-timeseries[,1:2]
names(timeseriesM)<-c("interval","avgsteps")
activityM<-merge(activity,timeseriesM,by="interval")
activityM<-activityM[with(activityM,order(date,interval)),]
        ## Step 2 If the steps is NA replace it with the AVG for that interval
activityM$stepsC<-activityM$steps
activityM$stepsC[is.na(activityM$stepsC)]<-activityM$avgsteps[is.na(activityM$stepsC)]
        ## Now Column stepsC has complete values if the steps is NA
head(activityM)
```

 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
```{r}
activityNew<-activityM[,c("stepsC","date","interval")]
names(activityNew)<-c("steps","date","interval")
head(activityNew)
```

 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean 
 and median total number of steps taken per day. Do these values differ from the estimates from the 
 first part of the assignment? What is the impact of imputing missing data on the estimates of the 
 total daily number of steps?
```{r}
totalstepsM<-ddply(activityNew,"date",summarise,steps=sum(steps))
hist(activityNew$steps,breaks=20,xlab="Steps per Day",main="Frequency of Steps Taken",col="blue")

avgNew<-mean(activityNew$steps)
medNew<-median(activityNew$steps)
avgmedNew<-data.frame(c("Mean","Median"),c(avgNew,medNew))
names(avgmedNew)<-c("Measure","Number")
print(avgmedNew)
```

## Are there differences in activity patterns between weekdays and weekends?

 1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating 
 whether a given date is a weekday or weekend day.
```{r}
library(chron)
activityNew$wkday<-as.factor(weekdays(as.Date(activityNew$date)))
wkend<-is.weekend(as.Date(activityNew$date))
activityNew$wkend<-wkend
activityNew$wkend<-as.factor(activityNew$wkend)
head(activityNew)
```

 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval 
 (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days 
 (y-axis). See the README file in the GitHub repository to see an example of what this plot should 
 look like using simulated data.

```{r}
library(plyr); library(dplyr)
timeseriesWkend<-ddply(activityNew,c("interval","wkend"),summarise,steps=mean(steps))
par(mfrow=c(2,1))
with(timeseriesWkend[timeseriesWkend$wkend=="TRUE",],plot(interval,steps,type="l",col="blue"))
with(timeseriesWkend[timeseriesWkend$wkend=="FALSE",],plot(interval,steps,type="l",col="blue"))
```
