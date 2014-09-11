# Reproducible Research: Peer Assessment 1
The following is an analysis of the data from a personal activity device. This analysis was conducted as a project for the Reproducible Reserach class offered through Coursera. The data was forked from the github repository <https://github.com/rdpeng/RepData_PeerAssessment1>, and the analysis was guided by specific questions that can be found in the README

## Loading and preprocessing the data

```r
# create a data frame from the read file, unless this step has 
# already been done
if (!exists("activitydata")){
        file<-unzip("activity.zip")
        activitydata<-read.csv(file = file, header = T, 
                               sep = ",", na.strings="NA")
}
```
The activitydata data frame spans 1/10/2012 through 30/11/2012. 


## What is mean total number of steps taken per day?  
1. *Make a histogram of the total number of steps taken each day*  


```r
# split the step record by day
allStepsByDay<-split(activitydata$steps, activitydata$date)
# sum all steps in each day
stepSum<-lapply(X = allStepsByDay, FUN = sum) 
# extract all values from list
totalStepsByDay<-matrix(unlist(stepSum),ncol=1) 
# plot histogram of extracted values
hist(totalStepsByDay, xlab = "total number of steps recorded each day", main = "Histogram of total recorded steps between 1/10/2012 and 30/11/2012")
```

![plot of chunk unnamed-chunk-2](./PA1_template_files/figure-html/unnamed-chunk-2.png) 
2. *Calculate and report the mean and median total number of steps taken per day*  
Calculating the mean is straightforward: 

```r
#for each day, calculate the mean of the steps taken
stepMeans<-lapply(X = allStepsByDay, function(x) mean(x, na.rm = T))
```
On any day, there are many five-minute intervals for which the number of steps recorded is 0. To accurately find the median number of steps taken *throughout the day*, these 0s must first be discarded. 

```r
nonZeroStepData<-activitydata
#change all 0 values in steps to NAs 
nonZeroStepData$steps[nonZeroStepData$steps==0]<-NA 
nonZeroStepsByDay<-split(nonZeroStepData$steps, nonZeroStepData$date)
```
Calculate the median as usual for each day

```r
stepMedians<-lapply(X = nonZeroStepsByDay, function(x) median(x, na.rm = T))
```
The following table presents the mean and median number of steps taken for each day

```r
meanAndMeds<-as.data.frame(cbind(stepMeans, stepMedians))
head(meanAndMeds, 10)
```

```
##            stepMeans stepMedians
## 2012-10-01       NaN          NA
## 2012-10-02    0.4375          63
## 2012-10-03     39.42          61
## 2012-10-04     42.07        56.5
## 2012-10-05     46.16          66
## 2012-10-06     53.54          67
## 2012-10-07     38.25        52.5
## 2012-10-08       NaN          NA
## 2012-10-09     44.48          48
## 2012-10-10     34.38        56.5
```


## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
