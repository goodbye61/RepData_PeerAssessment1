This is for Coursera Course Project 1.

Before the Instructions
-----------------------

1.First, load the data.

    data <- read.csv("activity.csv")
    str(data)

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

1.  Processing for 'date'

<!-- -->

    data$date <- as.Date(data$date, format = "%Y-%m-%d")
    str(data)

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

And then follow the instructions.

What is mean total number of steps taken per day ?
--------------------------------------------------

1.  The total number of steps taken per day.

<!-- -->

    sum <- with(data, tapply(steps,date,sum,na.rm=TRUE))
    sum

    ## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
    ##          0        126      11352      12116      13294      15420 
    ## 2012-10-07 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 
    ##      11015          0      12811       9900      10304      17382 
    ## 2012-10-13 2012-10-14 2012-10-15 2012-10-16 2012-10-17 2012-10-18 
    ##      12426      15098      10139      15084      13452      10056 
    ## 2012-10-19 2012-10-20 2012-10-21 2012-10-22 2012-10-23 2012-10-24 
    ##      11829      10395       8821      13460       8918       8355 
    ## 2012-10-25 2012-10-26 2012-10-27 2012-10-28 2012-10-29 2012-10-30 
    ##       2492       6778      10119      11458       5018       9819 
    ## 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 2012-11-05 
    ##      15414          0      10600      10571          0      10439 
    ## 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
    ##       8334      12883       3219          0          0      12608 
    ## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 
    ##      10765       7336          0         41       5441      14339 
    ## 2012-11-18 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 
    ##      15110       8841       4472      12787      20427      21194 
    ## 2012-11-24 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
    ##      14478      11834      11162      13646      10183       7047 
    ## 2012-11-30 
    ##          0

1.  Make a histogram of the total number of steps taken each day.
2.  Calculate and report the mean and median of the total number of
    steps taken per day

<!-- -->

    hist(sum)
    abline(v = mean(sum), col = "red", lwd = 4)
    abline(v = median(sum), col = "blue", lwd = 4)

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-4-1.png)

    ## Mean value:  9354.23

    ## Median value:  10395

On the figure above, there are two vertical lines which stand for mean
and median. Red line is for mean of sum, and blue line is for median of
sum.DF

What is the average daily activity pattern ?
--------------------------------------------

1.  Make a time series plot on every interval.

<!-- -->

    avg <- with(data,tapply(steps,interval,mean,na.rm=TRUE))
    plot(row.names(avg),avg,type = 'l', lwd = 3)

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-7-1.png)

1.  Which 5-minute interval on average across all the days in the
    dataset, contains the maximum numbers of step?

<!-- -->

    cat("Time interval which has the maximum numbers of step : ", row.names(avg)[which.max(avg)])

    ## Time interval which has the maximum numbers of step :  835

Imputing missing values
-----------------------

What is 'imputation' ?
======================

In statistics, imputation is the process of replacing missing data with
substituted values. - Wikipedia

1.  Calculate and report the total number of missing values in the
    dataset

<!-- -->

    cat("Total number of NAs: ", sum(sapply(data$steps, function(x) sum(length(which(is.na(x)))))))

    ## Total number of NAs:  2304

1.  Devise a strategy for filling in all of the missing values in the
    dataset. (could use the mean / median for that day, or the mean for
    that 5-minute interval,etc.)

    -&gt; I will impute the data with the mean for that 5-minute
    interval.

2.  Create a new dataset that is equal to the original dataset but with
    the missing data filled in.

<!-- -->

    avg_by_interval <- with(data,tapply(steps,interval,mean,na.rm =TRUE))
    num_of_rows <- dim(data)[1]  # the num of data. 
    ch <- data[c(1,3)]
    ch_date <- data[c(2)]
    s <- sapply(ch$steps, function(x) length(which(is.na(x))))
    names <- as.integer(names(avg_by_interval))

    z <- as.data.frame(avg_by_interval)
    z <- cbind(names,z) 

     # 's' stands for indicating which one is 'NAs' 
     # So , 's[i] == 1' means that we need to process on it. 

    for(i in 1:num_of_rows){
       
      if(s[i] == 1)
      {
        ch[i,1] <- subset(z$avg_by_interval , z$names == ch[i,2])
      }
      
    }


     # In 'ch' , steps & interval are configurated. 
     # Merging date and 'ch' 


    mrg <- cbind(ch,ch_date)  # Mrg indicates the new dataset with the missing data 
                              # filled in. 

<br>

1.  Make a histogram based on the new dataset.(mrg) What is the impact
    of imputing missing data on the estimates of the total daily number
    of steps?

<!-- -->

    # new dataset : mrg 
    s <- with(mrg, tapply(steps,date,sum,na.rm=TRUE))
    hist(s)

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-11-1.png)

    cat("Mean: ", mean(s))

    ## Mean:  10766.19

    cat("Median: ", median(s))

    ## Median:  10766.19

As shown above, histogram is different from first one. By imputing the
data, we could get more 'gaussian-likely' form. Because we got more data
by substituting NAs with real values. The more we get N, density will go
to the normal based on statistical reason.

Also, by imputing, I could extract same values with mean and median.

<br>

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

    mrg$days <- weekdays(mrg$date) # Adding new column indicating days. 
    head(mrg)

    ##       steps interval       date   days
    ## 1 1.7169811        0 2012-10-01 월요일
    ## 2 0.3396226        5 2012-10-01 월요일
    ## 3 0.1320755       10 2012-10-01 월요일
    ## 4 0.1509434       15 2012-10-01 월요일
    ## 5 0.0754717       20 2012-10-01 월요일
    ## 6 2.0943396       25 2012-10-01 월요일

Make 2 dataset. One for weekdays and the other for weekends.

1.  Create a new factor variable in the dataset with two levels-
    "weekday" and "weekend" indicating whether a given date is a weekday
    or weekend. <br> NOTICE: MY R OUTPUT ENVIRONMENT IS BASED ON
    'KOREAN'. I COULDN'T CHANGE MY ENVIRIONMENT ALTHOUGH I TRIED SEVERAL
    TRIALS.

    토요일: Saturday 일요일: Sunday

    <br>

<!-- -->

    w <- factor()

    for(i in 1:dim(mrg)[1]){

    if(mrg[i,]$days =="토요일" | mrg[i,]$days=="일요일"){
      w <- rbind(w,"weekend")
    }else{
      w <- rbind(w,"weekday") 
    }
    }

    mrg <- cbind(mrg,w) # mrg contains 'weekdays' column ! 

<br>

1.  Make panel plot containing a time series plot of the 5-minute
    interval. I'll use 'aggregate' function.

<!-- -->

    # Using aggregate function 

    avg_by_sw <- aggregate(steps ~ interval + w , data = mrg , mean) 
    library(lattice)

    xyplot(steps~ interval | w, data = avg_by_sw , type = 'l',  lwd = 2, layout = c(2,1), xlab = "Interval with every 5-minutes", ylab ="Mean of Steps", main =  "Steps ~ Interval", panel = function(x,y,...){
      
    panel.abline(h = mean(y), col = "red", lwd = 3)
    panel.xyplot(x,y,...)
      
    })

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-16-1.png)
<br>

The red horizontal line indicates mean of each steps of given time
interval. In general,as shown above, people walk more on weekends.
