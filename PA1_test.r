download.file('https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip'
              , 'activity.zip'
              , method='curl' )
              
unzip("activity.zip", files = NULL, exdir=".")

activity <- read.csv("activity.csv")

summary(activity)



