# libraries
library(ggplot2)

# get file
unzip("activity.zip")
raw <- read.csv("activity.csv", stringsAsFactors = FALSE)

#clean up data
  activity <- raw[complete.cases(raw), ]  #remove this?
  activity <- read.csv("activity.csv", stringsAsFactors = FALSE) # and do this instead
activity$date <- as.Date(activity$date, format = "%Y-%m-%d")


# get steps by Date
totalStepsByDate <- aggregate(steps ~ date, data = activity, sum)

# q1 - hist of steps
hist(totalStepsByDate$steps)
mean(totalStepsByDate$steps)
median(totalStepsByDate$steps)

#q2
averageStepsByInterval <- aggregate(steps ~ interval, data = activity, mean)
  averageStepsByInterval[averageStepsByInterval$steps == max(averageStepsByInterval$steps), ]
  averageStepsByInterval[averageStepsByInterval$steps == max(averageStepsByInterval$steps), "interval"]

#q3: imputation
activityImputed <- merge(activity, averageStepsByInterval, by.x = "interval", by.y = "interval")
names(activityImputed) <- c("interval", "steps", "date", "meanStepsByInterval")
activityImputed$steps[is.na(activityImputed$steps)] <- activityImputed[is.na(activityImputed$steps), "meanStepsByInterval"]
activityImputed$steps[is.na(activityImputed$steps)] <- round(activityImputed[is.na(activityImputed$steps), "meanStepsByInterval"], digits = 0)

# get steps by Date - imputed
totalStepsByDateImputed <- aggregate(steps ~ date, data = activityImputed, sum)

# q4 - hist of steps - imputed
hist(totalStepsByDateImputed$steps)
mean(totalStepsByDateImputed$steps)
median(totalStepsByDateImputed$steps)

#q5 - weekday
activityImputed$daytype <- factor(ifelse(weekdays(activityImputed$date) %in% c("Saturday", "Sunday"), "weekend", "weekday"))
averageStepsByIntervalAndDayType <- aggregate(steps ~ interval + daytype, data = activityImputed, mean)


print(
  qplot(
    interval, 
    steps, 
    data = averageStepsByIntervalAndDayType, 
    color = daytype, 
    facets = . ~ daytype, 
    geom = "line", 
    main = "title", 
    xlab = "interval", 
    ylab = "steps"
  )
)
