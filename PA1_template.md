# Reproducible Research: Peer Assessment 1



## Loading and preprocessing the data

Data is unzipped and the .csv file contained in the zip file is read into memory.


```r
unzip("activity.zip")
activity <- read.csv("activity.csv")
activity$date <- as.Date(activity$date)
# activity$time <- 
```

## What is mean total number of steps taken per day?

Use aggregate to get total number of steps per day and create historam and table output.


```r
activityDay <- aggregate(data=activity[,1:2], .~date, sum)
names(activityDay) <- c("Date", "TotalSteps")
hist(activityDay$TotalSteps, breaks=10, main="Histogram of total steps per day", xlab="Number of steps per day")
```

![](PA1_template_files/figure-html/meanSteps-1.png)<!-- -->

```r
meanSteps <- mean(activityDay$TotalSteps)
medianSteps <- median(activityDay$TotalSteps)

summarySteps <- data.frame(Mean = mean(activityDay$TotalSteps), Median = median(activityDay$TotalSteps))
names(summarySteps) <- c("Mean Steps","Median Steps")
kable(summarySteps, align = "c", format = "html", table.attr = "style='width:40%;'",
      caption = "Daily steps summary")
```

<table style='width:40%;'>
<caption>Daily steps summary</caption>
 <thead>
  <tr>
   <th style="text-align:center;"> Mean Steps </th>
   <th style="text-align:center;"> Median Steps </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:center;"> 10766.19 </td>
   <td style="text-align:center;"> 10765 </td>
  </tr>
</tbody>
</table>

## What is the average daily activity pattern?


```r
# activity<-merge(activity,activityDay,by.x="date", by.y="Date")

activityInterval <- aggregate(data=activity[,c(1,3)], .~interval, mean)

#plot(activity$interval,activity$steps, type='l')
ggplot(activityInterval) + geom_line(aes(interval,steps)) + 
    labs(title="Average daily activity level", x="5 minute interval", y="Average steps in interval")
```

![](PA1_template_files/figure-html/activityPattern-1.png)<!-- -->

```r
maxInd <- which.max(activityInterval$steps)
maxInt <- activityInterval$interval[maxInd]
maxTime <- paste(substr(maxInt,1,nchar(maxInt)-2),substr(maxInt,nchar(maxInt)-1,nchar(maxInt)), sep=":")
```

On average the maximum number of steps in a day occurs in the 104^th^ 5 minute interval which is labelled as 835 or 8:35.

## Imputing missing values

Missing values in the steps variable: 2304


```r
fillGap <- function(x) {
    
    if(is.na(x["steps"])) {
        as.integer(activityInterval$steps[activityInterval$interval==as.integer(x["interval"])])
    } else {
        as.integer(x["steps"])
    }
    
}

activity2<-activity
activity2$steps<-apply(activity,1,fillGap)

activityDay2 <- aggregate(data=activity2[,1:2], .~date, sum)
names(activityDay2) <- c("Date", "TotalSteps")
hist(activityDay2$TotalSteps, breaks=10, main="Histogram of total steps per day", xlab="Number of steps per day")
```

![](PA1_template_files/figure-html/fillGaps-1.png)<!-- -->

```r
meanSteps <- mean(activityDay2$TotalSteps)
medianSteps <- median(activityDay2$TotalSteps)

summarySteps2 <- data.frame(Mean = mean(activityDay$TotalSteps), Median = median(activityDay$TotalSteps))
names(summarySteps2) <- c("Mean Steps","Median Steps")
kable(summarySteps2, align = "c", format = "html", table.attr = "style='width:40%;'",
      caption = "Daily steps summary")
```

<table style='width:40%;'>
<caption>Daily steps summary</caption>
 <thead>
  <tr>
   <th style="text-align:center;"> Mean Steps </th>
   <th style="text-align:center;"> Median Steps </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:center;"> 10766.19 </td>
   <td style="text-align:center;"> 10765 </td>
  </tr>
</tbody>
</table>

## Are there differences in activity patterns between weekdays and weekends?


```r
wdays <- function(y) {
    ydate <- as.Date(y["date"])
    if (weekdays(ydate)=="Saturday" | weekdays(ydate)=="Sunday") {
        2
    } else {
        1
    }
}

activity2$wday <- factor(apply(activity2,1,wdays), labels = c("weekday", "weekend"))

activityIntervalWday <- aggregate(data=activity2[,c(1,3,4)], .~interval+wday, mean)

ggplot(activityIntervalWday) + geom_line(aes(interval,steps)) + facet_wrap("wday", ncol=1) +
    labs(title="Average daily activity level", x="5 minute interval", y="Average steps in interval")
```

![](PA1_template_files/figure-html/weekdays-1.png)<!-- -->


