download.file('https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip'
              , 'activity.zip'
              , method='curl' )
              
unzip("activity.zip", files = NULL, exdir=".")

activity <- read.csv("activity.csv")

summary(activity)

head(activity)

activity$date <- as.Date(activity$date)

good_act <- activity[complete.cases(activity), ]

# Calculate the total number of steps taken per day
steps_per_day <- aggregate(steps ~ date, good_act, sum)
steps_per_day

# Create a histogram of no of steps per day
hist(steps_per_day$steps, main = "Histogram of total number of steps per day", xlab = "Steps per day")

library(dplyr)
dfd <- activity %>% group_by(date) %>%
      summarise(steps.sum = sum(steps, na.rm = TRUE),
      steps.mean = mean(steps, na.rm = TRUE))

dfd

library(ggplot2)
ggplot(dfd, aes(x=steps.sum)) + 
geom_histogram(color="black", fill="white") +
geom_vline(aes(xintercept = mean(steps.sum)),col='goldenrod',size=1) +
geom_vline(aes(xintercept = median(steps.sum)),col='dodgerblue',size=1)




dfi <- activity %>% group_by(interval) %>%
      summarise(steps.mean = mean(steps, na.rm = TRUE))

ggplot(dfi,aes(x = interval, y = steps.mean)) +
geom_line() +
ggtitle("Average number of steps taken in a day")

dfi[dfi$steps.mean == max(dfi$steps.mean), ]


dfd
?is.na()
x <- c(1,2,NA,4,5)
x[is.na(x)] <- 0
x



activity[is.na(activity$steps), ]$date

date.has.na.steps <- activity[activity$date %in% activity[is.na(activity$steps), ]$date, ]
date.has.na.steps[is.na(date.has.na.steps$steps) == FALSE, ]



target.date <- activity$date[i]
        target.steps <- dfd[dfd$date == target.date, ]$steps.mean
        activity$steps[i] <- target.steps

for (i in 1:nrow(activity)) {
    if (is.na(activity$steps[i])) {
        activity$steps[i] <- dfd[dfd$date == activity$date[i], ]$steps.mean
    }
}
summary(activity)

dff <- activity
for (i in 1:nrow(dff)) {
    if (is.na(dff$steps[i])) {
        dff$steps[i] <- dfi[dfi$interval == dff$interval[i], ]$steps.mean
    }
}
summary(dff)

sum(is.na(activity$steps))


dffd <- dff %>% group_by(date) %>%
      summarise(steps.sum = sum(steps, na.rm = TRUE))
ggplot(dffd, aes(x=steps.sum)) + 
geom_histogram(color="black", fill="white") +
geom_vline(aes(xintercept = mean(steps.sum)),col='goldenrod',size=1) +
geom_vline(aes(xintercept = median(steps.sum)),col='dodgerblue',size=1)
summary(dffd)


dfwd$weekday <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")[as.POSIXlt(as.Date(dfwd$date))$wday+1]
dfwd$weekday <- ifelse(as.POSIXlt(as.Date(dfwd$date))$wday %in% c(0,6), "Weekend", "Weekday")
summary(dfwd)

dfwdi <- dfwd %>% group_by(weekday, interval) %>%
      summarise(steps.mean = mean(steps, na.rm = TRUE))

ggplot(dfwdi,aes(x = interval, y = steps.mean)) +
geom_line(stat = "identity", aes(color = weekday)) +
facet_grid(weekday ~. ) + 
ggtitle("Average number of steps taken in a day")