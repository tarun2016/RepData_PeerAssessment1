    library(dplyr)

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    library(chron)
    library(ggplot2)

    activity_data<-read.csv("activity.csv")

    activity_data$date<-as.Date(activity_data$date,"%Y-%m-%d")

    #Remove NA values
    activity_data1<-activity_data[complete.cases(activity_data),]
    a<-activity_data1%>%group_by(date)%>%summarize(steps=sum(steps))
    hist(a$steps,xlab = "Steps",ylab = "Frequency",main=" Histogram of Total number of steps taken each day")

![](PA1_template_files/figure-markdown_strict/Calculate%20Mean-1.png)

    a1<-mean(a$steps)
    a2<-median(a$steps)

Mean of the total number of steps taken per day
-----------------------------------------------

    a1

    ## [1] 10766.19

Median of the total number of steps taken per day
-------------------------------------------------

    a2

    ## [1] 10765

Average daily activity pattern
------------------------------

    a3<-activity_data1%>%group_by(interval)%>%summarize(steps=sum(steps))
    plot(a3$interval,a3$steps,type="l",xlab="Interval",ylab = "Steps",main = "Steps vs Time Interval")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-3-1.png)

Interval containing the maximum number of steps
===============================================

    a3[a3$steps==max(a3$steps),"interval"]

    ## # A tibble: 1 × 1
    ##   interval
    ##      <int>
    ## 1      835

Total number of missing values in the dataset
---------------------------------------------

    nrow(activity_data[is.na(activity_data$steps),])

    ## [1] 2304

Fill missing values with mean of that day
-----------------------------------------

    a4<-activity_data1%>%group_by(interval)%>%summarize(steps=mean(steps))
    merged_data<-merge(activity_data,a4,by.x = "interval",by.y = "interval")
    merged_data$steps<-ifelse(is.na(merged_data$steps.x),merged_data$steps.y,merged_data$steps.x)

New dataset that is equal to the original dataset but with the missing data filled in
-------------------------------------------------------------------------------------

    new_dataset<-merged_data[,c("date","interval","steps")]
    head(new_dataset)

    ##         date interval    steps
    ## 1 2012-10-01        0 1.716981
    ## 2 2012-11-23        0 0.000000
    ## 3 2012-10-28        0 0.000000
    ## 4 2012-11-06        0 0.000000
    ## 5 2012-11-24        0 0.000000
    ## 6 2012-11-15        0 0.000000

histogram of the total number of steps taken each day
-----------------------------------------------------

    new_hist<-new_dataset%>%group_by(date)%>%summarize(steps=sum(steps))
    hist(new_hist$steps,xlab="Steps",ylab="Frequency",main = "Histogram  Total number of steps taken each day")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-8-1.png)
\#\#Mean of total number of steps taken per day

    mean(new_hist$steps)

    ## [1] 10766.19

Median of total number of steps taken per day
---------------------------------------------

    median(new_hist$steps)

    ## [1] 10766.19

Impact of imputing missing data on the estimates of the total daily number of steps
-----------------------------------------------------------------------------------

    #For Mean
    mean(new_hist$steps)-a1

    ## [1] 0

    #For Median
    median(new_hist$steps)-a2

    ## [1] 1.188679

Differences in activity patterns between weekdays and weekends
--------------------------------------------------------------

    new_dataset$day<-ifelse(is.weekend(new_dataset$date),"weekend","weekday")
    new_plot<-new_dataset%>%group_by(interval,day)%>%summarize(steps=mean(steps))
    ggplot(new_plot,aes(x=interval,y=steps))+geom_line()+facet_wrap(~day)+labs(x="Interval",y="Steps")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-12-1.png)
