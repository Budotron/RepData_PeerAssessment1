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

2 *Process/transform the data (if necessary) into a format suitable for your analysis*  
All processing is simple enough to take place in-question

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
# Calculate the median as usual for each day
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
paste("The interval contains the maximum number of steps is", lower, "(to", upper, "), and the maximum average number of steps is", maxAv, sep = " ")
```

```
## [1] "The interval contains the maximum number of steps is 835 (to 840 ), and the maximum average number of steps is 206.169811320755"
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
2. replace NAs by the mean value of the interval if the ratio of mean to median in that interval is between 0.9 and 1.1  
3. otherwise, replace NAs with the median value of the interval  
However, many intervals will be composed of NAs only, perhaps indicating a period of regular non-movement (as in travelling to work). 

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
ratios<-avStepsByInterval/intervalStepMedians; ratios
```

```
##           [,1]
##   [1,] 0.05050
##   [2,] 0.01887
##   [3,] 0.01887
##   [4,] 0.01887
##   [5,] 0.01887
##   [6,] 0.08726
##   [7,] 0.01887
##   [8,] 0.01887
##   [9,]      NA
##  [10,] 0.03774
##  [11,] 0.03774
##  [12,] 0.01887
##  [13,] 0.01887
##  [14,] 0.01887
##  [15,] 0.01887
##  [16,] 0.01887
##  [17,]      NA
##  [18,] 0.01887
##  [19,] 0.07788
##  [20,] 0.01887
##  [21,] 0.03774
##  [22,] 0.03774
##  [23,] 0.01887
##  [24,]      NA
##  [25,]      NA
##  [26,]      NA
##  [27,] 0.03774
##  [28,]      NA
##  [29,]      NA
##  [30,] 0.01887
##  [31,]      NA
##  [32,] 0.01887
##  [33,]      NA
##  [34,]      NA
##  [35,] 0.08363
##  [36,] 0.01887
##  [37,]      NA
##  [38,]      NA
##  [39,]      NA
##  [40,]      NA
##  [41,] 0.01887
##  [42,] 0.01887
##  [43,] 0.04270
##  [44,] 0.05317
##  [45,] 0.03774
##  [46,] 0.01887
##  [47,]      NA
##  [48,]      NA
##  [49,] 0.03774
##  [50,] 0.03774
##  [51,] 0.06843
##  [52,]      NA
##  [53,] 0.03774
##  [54,] 0.01887
##  [55,] 0.09566
##  [56,] 0.03774
##  [57,] 0.09696
##  [58,] 0.05535
##  [59,] 0.09434
##  [60,] 0.12369
##  [61,]      NA
##  [62,] 0.07830
##  [63,] 0.11765
##  [64,] 0.06604
##  [65,] 0.07065
##  [66,] 0.06370
##  [67,] 0.05984
##  [68,] 0.24226
##  [69,] 0.59329
##  [70,] 0.49567
##  [71,] 0.58885
##  [72,] 1.11226
##  [73,] 0.57256
##  [74,] 2.40313
##  [75,] 2.68868
##  [76,] 1.56674
##  [77,] 0.99925
##  [78,] 1.25535
##  [79,] 1.49003
##  [80,] 0.99594
##  [81,] 1.04807
##  [82,] 1.29911
##  [83,] 1.13208
##  [84,] 1.14041
##  [85,] 1.41327
##  [86,] 1.10943
##  [87,] 1.44313
##  [88,] 1.09019
##  [89,] 1.13465
##  [90,] 1.49945
##  [91,] 0.73262
##  [92,] 0.87764
##  [93,] 1.00508
##  [94,] 1.59879
##  [95,] 1.08129
##  [96,] 1.23409
##  [97,] 1.32211
##  [98,] 0.97439
##  [99,] 1.29434
## [100,] 1.29122
## [101,] 2.19424
## [102,] 1.50870
## [103,] 1.36386
## [104,] 0.81490
## [105,] 0.90706
## [106,] 0.74509
## [107,] 0.45508
## [108,] 0.89315
## [109,] 1.07054
## [110,] 0.85543
## [111,] 1.38998
## [112,] 0.82529
## [113,] 1.38289
## [114,] 0.68058
## [115,] 1.27322
## [116,] 0.77977
## [117,] 0.45077
## [118,] 0.88079
## [119,] 0.44848
## [120,] 0.25217
## [121,] 0.35899
## [122,] 0.19341
## [123,] 0.26676
## [124,] 0.28620
## [125,] 0.28412
## [126,] 0.43787
## [127,] 0.50322
## [128,] 0.59864
## [129,] 0.22242
## [130,] 0.65906
## [131,] 0.51213
## [132,] 0.75161
## [133,] 0.95026
## [134,] 0.91321
## [135,] 0.67685
## [136,] 0.35482
## [137,] 0.58510
## [138,] 0.43043
## [139,] 0.61915
## [140,] 0.69418
## [141,] 0.92391
## [142,] 1.11509
## [143,] 0.85255
## [144,] 0.73986
## [145,] 0.65843
## [146,] 0.79007
## [147,] 1.11587
## [148,] 0.99757
## [149,] 0.92549
## [150,] 0.63506
## [151,] 0.76721
## [152,] 0.85303
## [153,] 0.55849
## [154,] 0.49008
## [155,] 1.00126
## [156,] 0.48756
## [157,] 0.64151
## [158,] 0.53182
## [159,] 0.43264
## [160,] 0.26611
## [161,] 0.92491
## [162,] 0.46448
## [163,] 0.48039
## [164,] 0.44091
## [165,] 0.63940
## [166,] 0.71875
## [167,] 0.60281
## [168,] 0.63677
## [169,] 0.57777
## [170,] 0.88072
## [171,] 0.52197
## [172,] 1.05865
## [173,] 0.93347
## [174,] 0.40812
## [175,] 0.89041
## [176,] 0.56142
## [177,] 0.36803
## [178,] 0.41064
## [179,] 0.94832
## [180,] 0.75472
## [181,] 0.40025
## [182,] 0.48751
## [183,] 0.51811
## [184,] 0.48260
## [185,] 0.90122
## [186,] 0.79591
## [187,] 0.64176
## [188,] 0.56069
## [189,] 0.25827
## [190,] 0.57361
## [191,] 0.38031
## [192,] 0.87008
## [193,] 1.17230
## [194,] 1.05135
## [195,] 1.10440
## [196,] 0.96442
## [197,] 1.80653
## [198,] 0.63929
## [199,] 1.43829
## [200,] 0.76368
## [201,] 0.79751
## [202,] 0.72147
## [203,] 0.94301
## [204,] 0.76630
## [205,] 1.13714
## [206,] 0.79298
## [207,] 0.67175
## [208,] 1.00371
## [209,] 0.94438
## [210,] 0.98679
## [211,] 1.60333
## [212,] 0.87736
## [213,] 1.19197
## [214,] 0.96597
## [215,] 0.57006
## [216,] 0.87100
## [217,] 0.81358
## [218,] 1.45047
## [219,] 1.40940
## [220,] 1.64078
## [221,] 1.16204
## [222,] 1.16851
## [223,] 1.31692
## [224,] 1.31408
## [225,] 1.42233
## [226,] 1.65755
## [227,] 1.63368
## [228,] 1.88140
## [229,] 1.30566
## [230,] 1.36544
## [231,] 1.34971
## [232,] 1.15997
## [233,] 0.77278
## [234,] 0.55245
## [235,] 0.53197
## [236,] 0.78468
## [237,] 0.54428
## [238,] 0.65506
## [239,] 0.87808
## [240,] 0.84882
## [241,] 0.56065
## [242,] 0.50050
## [243,] 0.46047
## [244,] 0.90107
## [245,] 0.58285
## [246,] 0.70566
## [247,] 0.44757
## [248,] 0.48499
## [249,] 0.48868
## [250,] 0.53302
## [251,] 0.93629
## [252,] 0.33585
## [253,] 0.41956
## [254,] 0.53833
## [255,] 0.36081
## [256,] 0.38109
## [257,] 0.20085
## [258,] 0.44549
## [259,] 0.34495
## [260,] 0.17816
## [261,] 0.25527
## [262,] 0.13671
## [263,] 0.12608
## [264,] 0.15427
## [265,] 0.08546
## [266,] 0.10821
## [267,] 0.07953
## [268,] 0.12244
## [269,] 0.14589
## [270,] 0.41420
## [271,] 0.60967
## [272,] 0.03774
## [273,] 0.01887
## [274,] 0.01887
## [275,] 0.08910
## [276,] 0.03774
## [277,] 0.25399
## [278,] 0.08380
## [279,]      NA
## [280,] 0.03774
## [281,] 0.01887
## [282,] 0.13208
## [283,] 0.08012
## [284,] 0.10213
## [285,] 0.05159
## [286,] 0.03774
## [287,] 0.03774
## [288,] 0.05377
```
## Are there differences in activity patterns between weekdays and weekends?
