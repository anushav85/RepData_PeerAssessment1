#REPRODUCIBLE RESEARCH Peer Assessment 1
#Load the data and preprocess it for analysis
unzip("activity.zip")
activity <- read.csv("activity.csv")

#Find the mean total no. of stepss per day
activity_NAomit <- na.omit(activity)
sum_stepsperday <- aggregate(activity_NAomit$steps,list(activity_NAomit$date),sum)
sum_stepsperday$Group.1 <- (as.Date(sum_stepsperday$Group.1))
png("fig1.png")
hist(as.numeric(sum_stepsperday$x),xlab = "Steps per day", main = "Sum of steps per day without NAs")
dev.off()
mean_stepsperday <- mean(sum_stepsperday$x)
median_stepsperday <- median(sum_stepsperday$x)

#Find the average daily activity pattern
avgsteps_interval <- aggregate(activity_NAomit$steps, list(activity_NAomit$interval),mean)
png("fig2.png")
plot(avgsteps_interval$Group.1,avgsteps_interval$x, xlab = "Interval",ylab = "Avg no. of steps", main = "Avg Steps Vs Interval", type = "l")
dev.off()
avgsteps_max <- max(avgsteps_interval$x)
int_max <- avgsteps_interval$Group.1[match(avgsteps_max,avgsteps_interval$x)]

#Imputing missing values
NAtotal <- sum(is.na(activity))
library(Hmisc)
activity$nanewval <- rep(avgsteps_interval$x, 61)
subset <- subset(activity, is.na(steps)== TRUE)
subset$steps <- impute(subset$steps,subset$nanewval)
activity$steps <- impute(activity$steps, subset$steps)
#imputed <- sum(is.imputed(activity$steps))
activity_imputed <- activity[1:3]
sum_stepsperdayi <- aggregate(activity_imputed$steps,list(activity_imputed$date),sum)
sum_stepsperdayi$Group.1 <- (as.Date(sum_stepsperdayi$Group.1))
png("fig3.png")
hist(as.numeric(sum_stepsperdayi$x),xlab = "Steps per day", main = "Sum of steps per day with Imputation")
dev.off()
mean_stepsperdayi <- mean(sum_stepsperdayi$x)
median_stepsperdayi <- median(sum_stepsperdayi$x)

#Find the differences in the activity patterns between
#weekdays and weekends
activity_imputed$date <- as.Date(activity_imputed$date)
activity_imputed$daytype <- weekdays(activity_imputed$date)
activity_imputed$daytype <- factor(activity_imputed$daytype, levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
activity_imputed$daytype <- as.numeric(activity_imputed$daytype)
for(i in 1:17568){
  if(activity_imputed$daytype[i]<6)
    {
    activity_imputed$daytype[i]="weekday"
  }
  else
  {
    activity_imputed$daytype[i]="weekend"
  }
}
weekday_activity <- subset(activity_imputed, daytype=="weekday")
weekend_activity <- subset(activity_imputed, daytype=="weekend")
avgsteps_intweekday <- aggregate(weekday_activity$steps, list(weekday_activity$interval),mean)
avgsteps_intweekend <- aggregate(weekend_activity$steps, list(weekend_activity$interval),mean)
#Make a panel plot to show the difference between
#activity patterns between weekdays and weekends
png("fig4.png")
par(mfrow = c(2,1))
plot(avgsteps_intweekday$Group.1,avgsteps_intweekday$x, xlab = "Interval",ylab = "Number of steps", main = "weekday", type = "l")
plot(avgsteps_intweekend$Group.1,avgsteps_intweekend$x, xlab = "Interval",ylab = "Number of steps", main = "weekend", type = "l")
dev.off()



