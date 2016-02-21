#load required libraries
# load ggplot2 for e.g. the histogram
library(ggplot2)

if(!file.exists('activity.csv')){
  unzip('activity.zip')
}
activityData <- read.csv('activity.csv')


#activityData$interval <- strptime(gsub("([0-9]{1,2})([0-9]{2})", "\\1:\\2", activityData$interval), format='%H:%M')

stepsPerDay <- tapply(activityData$steps, activityData$date, sum, na.rm=TRUE)

qplot(stepsPerDay, xlab='Steps per day', ylab='Frequency using binwith 1000', binwidth=1000)

stepsPerDayMean <- mean(stepsByDay)
stepsPerDayMedian <- median(stepsByDay)