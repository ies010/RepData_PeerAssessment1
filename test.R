library(knitr)
library(dplyr)
library(ggplot2)

# Loading and preprocessing the data
# Load the data:
filename <- 'activity.csv'
if (!file.exists(filename)) {unzip("activity.zip")}
activityData <- read.csv("activity.csv", colClass=c('integer', 'Date', 'integer'))


## Q1. What is mean total number of steps taken per day?
# For this part of the assignment the missing values can be ignored.

# 1. Calculate the total number of steps taken per day.
totalStepsPerDate <- aggregate(steps ~ date, activityData, sum)

# 2. Make a histogram of the total number of steps taken each day.
ggplot(totalStepsPerDate, aes(x = steps)) +
  geom_histogram(fill = 'red', binwidth = 1000) +
  labs(title = 'Histogram: Total steps per day', 
       x = 'Steps per day', 
       y = 'Frequency')

# 3. Calculate and report the mean and median of the total number of steps taken per day.
meanSteps <- mean(totalStepsPerDate$steps, na.rm = TRUE)
medianSteps <- median(totalStepsPerDate$steps, na.rm = TRUE)

# The mean is and the median is


## Q2. What is the average daily activity pattern?

# 1. Make a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and
# the average number of steps taken, averaged across all days (y-axis)
stepsPerInterval <- aggregate(steps ~ interval, activityData, mean)
ggplot(stepsPerInterval, aes(x=interval, y=steps)) +
  geom_line(color = 'red')

# 2. Which 5-minute interval, on average across all the days in the dataset, 
# contains the maximum number of steps?
steps.interval$interval[which.max(steps.interval$steps)]


# 3. Imputing missing values

# 1. Calculate and report the total number of missing values in the dataset (i.e. the total 
# number of rows with NAs)
sum(is.na(activityData$steps))

# 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy 
# does not need to be sophisticated. For example, you could use the mean/median for that day, 
# or the mean for that 5-minute interval, etc.
filledData <- data
nasIndex <- is.na(filledData$steps)
meanInterval <- tapply(filledData$steps, filledData$interval, mean, na.rm=TRUE, simplify=TRUE)

# 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
filledData$steps[nasIndex] <- meanInterval[as.character(filledData$interval[nasIndex])]

# 4. Make a histogram of the total number of steps taken each day and Calculate and report the 
# mean and median total number of steps taken per day. Do these values differ from the estimates 
# from the first part of the assignment? What is the impact of imputing missing data on the 
# estimates of the total daily number of steps?
totalStepsPerDay <- filledData %>%
    group_by(date) %>%
    summarize(steps = sum(steps)) %>%
    print
