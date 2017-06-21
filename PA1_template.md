# Reproducible Research: Peer Assessment 1



## Loading and preprocessing the data

Data is unzipped and the .csv file contained in the zip file is read into memory.


```r
unzip("activity.zip")
activity <- read.csv("activity.csv")
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
kable(summarySteps, align = "c", format = "html", table.attr = "style='width:40%;'")
```

<table style='width:40%;'>
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



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
