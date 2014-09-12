# Reproducible Research: Peer Assessment 1
The following is an analysis of the data from a personal activity device. This analysis was conducted as a project for the Reproducible Reserach class offered through Coursera. The data was forked from the github repository <https://github.com/rdpeng/RepData_PeerAssessment1>, and the analysis was guided by specific questions that can be found in the README

## Loading and preprocessing the data
1. *Load the data (i.e. read.csv())*

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
stepSumByDay<-lapply(X = allStepsByDay, FUN = sum) 
# extract all values from list
totalStepsByDay<-matrix(unlist(stepSumByDay),ncol=1) 
# plot histogram of extracted values
hist(totalStepsByDay, xlab = "total number of steps recorded each day", main = "Histogram of total recorded steps between 1/10/2012 and 30/11/2012")
```

![plot of chunk unnamed-chunk-2](./PA1_template_files/figure-html/unnamed-chunk-2.png) 

2. *Calculate and report the mean and median total number of steps taken per day*  

```r
#for each day, calculate the mean of the steps taken
stepMeans<-lapply(X = allStepsByDay, function(x) mean(x, na.rm = T))
```
On any day, there are many five-minute intervals for which the number of steps recorded is 0. To accurately find the median number of steps taken *throughout the day*, these 0s must first be discarded. 

```r
#copy activitydata to nonZeroData
nonZeroStepData<-activitydata
#change all 0 values in nonZeroData$steps to NAs 
nonZeroStepData$steps[nonZeroStepData$steps==0]<-NA 
nonZeroStepsByDay<-split(nonZeroStepData$steps, nonZeroStepData$date)
# Calculate the median for each day
stepMedians<-lapply(X = nonZeroStepsByDay, function(x) median(x, na.rm = T))
```

The following table presents the mean and median number of steps taken for each day

```r
meanAndMeds<-as.data.frame(cbind(stepMeans, stepMedians))
head(meanAndMeds, 5)
```

```
##            stepMeans stepMedians
## 2012-10-01       NaN          NA
## 2012-10-02    0.4375          63
## 2012-10-03     39.42          61
## 2012-10-04     42.07        56.5
## 2012-10-05     46.16          66
```


## What is the average daily activity pattern?

1. *Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)*

```r
dataByInterval<-split(activitydata$steps, activitydata$interval)
avStepsByInterval<-lapply(dataByInterval, function(x) mean(x, na.rm = T))
plot(x = unique(activitydata$interval),y = avStepsByInterval, type="l", xlab = "Five Minute Intervals", ylab = "Average number of steps taken", main = "Time series plot of interval vs average steps")
```

![plot of chunk unnamed-chunk-6](./PA1_template_files/figure-html/unnamed-chunk-6.png) 

2. *Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?*


```r
# get the values in the list avStepsByInterval 
avStepsByInterval<-matrix(unlist(avStepsByInterval),ncol=1)
# find the max of all the averages
maxAv<-max(avStepsByInterval)
# locate the index of this max value
indMax<-which(avStepsByInterval %in% maxAv)
# use the index to find the interval
lower<-unique(activitydata$interval)[indMax]
upper<-lower+5
paste("The interval containing the maximum number of steps is", lower, "(to", upper, "), and the maximum average number of steps is", maxAv, sep = " ")
```

```
## [1] "The interval containing the maximum number of steps is 835 (to 840 ), and the maximum average number of steps is 206.169811320755"
```

## Imputing missing values
1. *Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)*  

It makes sense that NAs only occur in steps, with all other data being automatically recorded by the device. Thus, the total number of rows with NAs is given by

```r
sum(is.na(activitydata$steps))
```

```
## [1] 2304
```
2. *Devise a strategy for filling in all of the missing values in the dataset. *  

Recalling the heuristics that  
-the median is most useful for describing the center of skewed distributions  
-the mean is most useful for describing symmetric distributions  
-the ratio of the mean to the median is a measure of skewness, ie: when $\frac{mean}{median}\approx1$, the distribution is symmetric; else, it is left- or right-skewed

the following imputation strategy is proposed:  
1. in each interval, use the ratio of mean to median to decide the skewness of the distribution  
2. replace NAs by the mean value of the interval if the ratio of mean to median in that interval is between 0.8 and 1.2  
3. otherwise, replace NAs with the median value of the interval  
However, after processing, many intervals will be composed of NAs only, perhaps indicating a period of regular non-movement (as in travelling to work). 

```r
# split nonZeroStepData (previously computed) to get data by interval
nonZeroStepsByInterval<-split(nonZeroStepData$steps, nonZeroStepData$interval)
nonZeroStepsByInterval[9]
```

```
## $`40`
##  [1] NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
## [24] NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
## [47] NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
```
In such cases, the median will be NA, and so will the ratio of mean to median. Then, NAs are replaced by the mean. 

```r
# Calculate the median for each interval
intervalStepMedians<-lapply(X = nonZeroStepsByInterval, function(x) median(x, na.rm = T))
#calculate the ratio of mean to median
intervalStepMedians<-matrix(unlist(intervalStepMedians), ncol = 1)
ratios<-as.vector(avStepsByInterval/intervalStepMedians)
for (i in (1:length(ratios))){
        if (is.na(ratios[i])){
                dataByInterval[[i]][which(is.na(dataByInterval[[i]]) %in% T)]<-avStepsByInterval[i]
        }else if(ratios[i] < 0.8){ 
                dataByInterval[[i]][which(is.na(dataByInterval[[i]]) %in% T)]<-avStepsByInterval[i]
        }else if (ratios[i] > 1.2){
                dataByInterval[[i]][which(is.na(dataByInterval[[i]]) %in% T)]<-avStepsByInterval[i]
        }else {
             dataByInterval[[i]][which(is.na(dataByInterval[[i]]) %in% T)]<-intervalStepMedians[i]
        }
}
#check that the missing data is successfully imputed
dataByInterval[10]
```

```
## $`45`
##  [1]  1.472  0.000  0.000  0.000  0.000  0.000  0.000  1.472  0.000  0.000
## [11]  0.000  0.000  0.000  0.000  0.000  0.000 72.000  0.000  0.000  6.000
## [21]  0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000
## [31]  0.000  1.472  0.000  0.000  1.472  0.000  0.000  0.000  0.000  1.472
## [41]  1.472  0.000  0.000  0.000  1.472  0.000  0.000  0.000  0.000  0.000
## [51]  0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000
## [61]  1.472
```

3. *Create a new dataset that is equal to the original dataset but with the missing data filled in.*

```r
allsteps<-unsplit(dataByInterval, activitydata$interval)
dates<-activitydata$date
interval<-activitydata$interval
newdf<-data.frame(allsteps, dates, interval)
```
4. *Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day*  

```r
# split the step record by day
newAllStepsByDay<-split(newdf$allsteps, newdf$date)
# sum all steps in each day
newStepSumByDay<-lapply(X = newAllStepsByDay, FUN = sum) 
# extract all values from list
newTotalStepsByDay<-matrix(unlist(newStepSumByDay),ncol=1) 
# plot histogram of extracted values
hist(newTotalStepsByDay, xlab = "total number of steps recorded each day", main = "Total recorded steps between 1/10/2012 and 30/11/2012 (NA replaced)")
```

![plot of chunk unnamed-chunk-12](./PA1_template_files/figure-html/unnamed-chunk-12.png) 

```r
#for each day, calculate the mean of the steps taken
newStepMeans<-lapply(X = newAllStepsByDay, function(x) mean(x, na.rm = T))
#copy activitydata to nonZeroData
newNonZeroStepData<-newdf
#change all 0 values in nonZeroData$steps to NAs 
newNonZeroStepData$steps[newNonZeroStepData$allsteps==0]<-NA 
newNonZeroStepsByDay<-split(newNonZeroStepData$allsteps, 
                            newNonZeroStepData$date)
# Calculate the median for each day
newStepMedians<-lapply(X = newNonZeroStepsByDay, 
                       function(x) median(x, na.rm = T))
newMeanAndMeds<-as.data.frame(cbind(newStepMeans, newStepMedians, stepMeans, stepMedians))
head(newMeanAndMeds, 200)
```

```
##            newStepMeans newStepMedians stepMeans stepMedians
## 2012-10-01        37.86          34.88       NaN          NA
## 2012-10-02       0.4375              0    0.4375          63
## 2012-10-03        39.42              0     39.42          61
## 2012-10-04        42.07              0     42.07        56.5
## 2012-10-05        46.16              0     46.16          66
## 2012-10-06        53.54              0     53.54          67
## 2012-10-07        38.25              0     38.25        52.5
## 2012-10-08        37.86          34.88       NaN          NA
## 2012-10-09        44.48              0     44.48          48
## 2012-10-10        34.38              0     34.38        56.5
## 2012-10-11        35.78              0     35.78          35
## 2012-10-12        60.35              0     60.35          46
## 2012-10-13        43.15              0     43.15        45.5
## 2012-10-14        52.42              0     52.42        60.5
## 2012-10-15         35.2              0      35.2          54
## 2012-10-16        52.38              0     52.38          64
## 2012-10-17        46.71              0     46.71        61.5
## 2012-10-18        34.92              0     34.92        52.5
## 2012-10-19        41.07              0     41.07          74
## 2012-10-20        36.09              0     36.09          49
## 2012-10-21        30.63              0     30.63          48
## 2012-10-22        46.74              0     46.74          52
## 2012-10-23        30.97              0     30.97          56
## 2012-10-24        29.01              0     29.01        51.5
## 2012-10-25        8.653              0     8.653          35
## 2012-10-26        23.53              0     23.53        36.5
## 2012-10-27        35.14              0     35.14          72
## 2012-10-28        39.78              0     39.78          61
## 2012-10-29        17.42              0     17.42        54.5
## 2012-10-30        34.09              0     34.09          40
## 2012-10-31        53.52              0     53.52        83.5
## 2012-11-01        37.86          34.88       NaN          NA
## 2012-11-02        36.81              0     36.81        55.5
## 2012-11-03         36.7              0      36.7          59
## 2012-11-04        37.86          34.88       NaN          NA
## 2012-11-05        36.25              0     36.25          66
## 2012-11-06        28.94              0     28.94          52
## 2012-11-07        44.73              0     44.73          58
## 2012-11-08        11.18              0     11.18        42.5
## 2012-11-09        37.86          34.88       NaN          NA
## 2012-11-10        37.86          34.88       NaN          NA
## 2012-11-11        43.78              0     43.78          55
## 2012-11-12        37.38              0     37.38          42
## 2012-11-13        25.47              0     25.47          57
## 2012-11-14        37.86          34.88       NaN          NA
## 2012-11-15       0.1424              0    0.1424        20.5
## 2012-11-16        18.89              0     18.89          43
## 2012-11-17        49.79              0     49.79        65.5
## 2012-11-18        52.47              0     52.47          80
## 2012-11-19         30.7              0      30.7          34
## 2012-11-20        15.53              0     15.53          58
## 2012-11-21         44.4              0      44.4          55
## 2012-11-22        70.93              0     70.93          65
## 2012-11-23        73.59              0     73.59         113
## 2012-11-24        50.27              0     50.27        65.5
## 2012-11-25        41.09              0     41.09          84
## 2012-11-26        38.76              0     38.76          53
## 2012-11-27        47.38              0     47.38          57
## 2012-11-28        35.36              0     35.36          70
## 2012-11-29        24.47              0     24.47        44.5
## 2012-11-30        37.86          34.88       NaN          NA
```

## Are there differences in activity patterns between weekdays and weekends?
