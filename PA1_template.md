Reproducible Research: Peer Assessment 1
========================

###1. Code for reading in the dataset and/or processing the data


```r
data <- read.csv("activity.csv", header = TRUE, colClasses = c("double","character","double"), na.strings = "NA")
```

```
## Warning in file(file, "rt"): cannot open file 'activity.csv': No such file
## or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

```r
head(data)
```

```
##                                                                      
## 1 function (..., list = character(), package = NULL, lib.loc = NULL, 
## 2     verbose = getOption("verbose"), envir = .GlobalEnv)            
## 3 {                                                                  
## 4     fileExt <- function(x) {                                       
## 5         db <- grepl("\\\\.[^.]+\\\\.(gz|bz2|xz)$", x)              
## 6         ans <- sub(".*\\\\.", "", x)
```

```r
summary(data)
```

```
## Error in object[[i]]: object of type 'closure' is not subsettable
```

###2. Histogram of the total number of steps taken each day  


```r
histData <- aggregate(data$steps, list(date = data$date), FUN=sum, na.rm=TRUE)
```

```
## Error in data$steps: object of type 'closure' is not subsettable
```

```r
with(histData, hist(x, col = "green", breaks=20,xlab = "Number of Steps Taken Per Day"))
```

```
## Error in with(histData, hist(x, col = "green", breaks = 20, xlab = "Number of Steps Taken Per Day")): object 'histData' not found
```

Summary Statistics including mean and meadian for number of steps taken per day


```r
summary(histData$x)
```

```
## Error in summary(histData$x): object 'histData' not found
```

###3. What is the average daily activity pattern?

Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
timeData <-  aggregate(data$steps, list(interval=data$interval), FUN=mean, na.rm=TRUE)
```

```
## Error in data$steps: object of type 'closure' is not subsettable
```

```r
head(timeData)
```

```
## Error in head(timeData): object 'timeData' not found
```

```r
with(timeData, plot(interval, x, main = "Average number of steps taken per 5 minute interval", ylab = "Number of steps taken", xlab = "5 minute interval"))
```

```
## Error in with(timeData, plot(interval, x, main = "Average number of steps taken per 5 minute interval", : object 'timeData' not found
```

```r
lines(timeData$interval, timeData$x, type = "l")
```

```
## Error in lines(timeData$interval, timeData$x, type = "l"): object 'timeData' not found
```

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
timeData[timeData$x == max(timeData$x, na.rm = TRUE),]
```

```
## Error in eval(expr, envir, enclos): object 'timeData' not found
```

###4. Imputing missing values

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
naCount <-  rowSums(is.na(data))
```

```
## Warning in is.na(data): is.na() applied to non-(list or vector) of type
## 'closure'
```

```
## Error in rowSums(is.na(data)): 'x' must be an array of at least two dimensions
```

```r
table(naCount)
```

```
## Error in table(naCount): object 'naCount' not found
```
There are 2304 NA rows in this dataset

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
filler <-  function(steps, interval) {

        if (!is.na(steps))
                fillValue <- steps
        else
                fillValue <- mean(timeData[timeData$interval==interval,"x"], na.rm = TRUE)
        return(fillValue)
}

filledData <- data
filledData$steps <- mapply(filler, filledData$steps, filledData$interval)
```

```
## Error in filledData$steps: object of type 'closure' is not subsettable
```

```r
head(filledData)
```

```
##                                                                      
## 1 function (..., list = character(), package = NULL, lib.loc = NULL, 
## 2     verbose = getOption("verbose"), envir = .GlobalEnv)            
## 3 {                                                                  
## 4     fileExt <- function(x) {                                       
## 5         db <- grepl("\\\\.[^.]+\\\\.(gz|bz2|xz)$", x)              
## 6         ans <- sub(".*\\\\.", "", x)
```


Make a histogram of the total number of steps taken each day. 


```r
histFilledData <- aggregate(filledData$steps, list(date = filledData$date), FUN=sum, na.rm=TRUE)
```

```
## Error in filledData$steps: object of type 'closure' is not subsettable
```

```r
with(histFilledData, hist(x, col = "green", breaks=20,xlab = "Number of Steps Taken Per Day", main = "Total Number of Steps Taken Per Day"))
```

```
## Error in with(histFilledData, hist(x, col = "green", breaks = 20, xlab = "Number of Steps Taken Per Day", : object 'histFilledData' not found
```


Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
summary(histFilledData$x)
```

```
## Error in summary(histFilledData$x): object 'histFilledData' not found
```

The median number of steps taken per day increased 10400 -> 10770 and the mean increased 9354  -> 10770. Now the mean = median

###5. Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
filledData$dateType <-  ifelse(as.POSIXlt(filledData$date)$wday %in% c(0,6), 'weekend', 'weekday')
```

```
## Error in filledData$date: object of type 'closure' is not subsettable
```

```r
head(filledData)
```

```
##                                                                      
## 1 function (..., list = character(), package = NULL, lib.loc = NULL, 
## 2     verbose = getOption("verbose"), envir = .GlobalEnv)            
## 3 {                                                                  
## 4     fileExt <- function(x) {                                       
## 5         db <- grepl("\\\\.[^.]+\\\\.(gz|bz2|xz)$", x)              
## 6         ans <- sub(".*\\\\.", "", x)
```

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.






```r
histFilledData2 <- aggregate(filledData$steps, list(interval = filledData$interval, dateType = filledData$dateType), FUN=mean, na.rm=TRUE)
```

```
## Error in filledData$steps: object of type 'closure' is not subsettable
```

```r
library(lattice)
```

```
## Warning: package 'lattice' was built under R version 3.2.5
```

```r
xyplot(x ~interval | dateType, data = histFilledData2, layout = c(1,2), ylab = "Number of Steps", xlab = "Interval", type = "b")
```

```
## Error in eval(substitute(groups), data, environment(x)): object 'histFilledData2' not found
```
