# This file / script describes the steps involved in the analysis of the data. The
# code is minimally commented, since the .Rmd file contains a thorough description.

# Exploratory analyses; where are the NA's?
plot(activity$steps)
plot(is.na(activity$steps))

# It looks like data is missing for some consecutive intervals.

##### QUESTION #1 ######

# What is mean total number of steps taken per day?
mean_total_steps <- activity %>%
  # First group activity by date...
  group_by(date) %>%
    # Then summarise by summing steps...
    summarise (daily_steps = sum(steps)) %>%
      # Summarise again by averaging daily steps...
      # Use na.rm = TRUE, otherwise the result is NA...
      summarise(mean_total_steps = mean(daily_steps, na.rm = TRUE))

# Result:
mean_total_steps
# ...10766.19 steps on average per day.


# What is median total number of steps taken per day?
median_total_steps <- activity %>%
  # First group activity by date...
  group_by(date) %>%
  # Then summarise by summing steps...
  summarise (daily_steps = sum(steps)) %>%
  # Summarise again by averaging daily steps...
  # Use na.rm = TRUE, otherwise the result is NA...
  summarise(median_total_steps = median(daily_steps, na.rm = TRUE))

# Result:
median_total_steps
# ...10765 steps on average per day.


# What about the missing data? This code calculates the number of
# days with NA as a value:
steps_by_day <- activity %>%
  # First group activity by date...
  group_by(date) %>%
  # Then summarise by summing steps...
  summarise (daily_steps = sum(steps))

# Now count the number of days with NA results:
NA_days <- sum(is.na(steps_by_day$daily_steps))
# Result:
NA_days
# ...8 days with missing data.

# What proportion of the total is that?
NA_proportion <- NA_days / nrow(steps_by_day)
# Result:
NA_proportion
# ...0.1311475 - about 13% of data is missing.


# Histogram of total steps taken per day:
hist(steps_by_day$daily_steps)
# So, on most days the number of steps lies between 10.000 and 15.000...

# An alternative way of visualizing that is by means of a boxplot:
boxplot(steps_by_day$daily_steps)


##### QUESTION #2 ######

# What is the average daily activity pattern?
steps_by_interval <- activity %>%
  # First group activity by date...
  group_by(interval) %>%
  # Then summarise by summing steps...
  summarise (av_steps = mean(steps, na.rm = TRUE))

# Result:
steps_by_interval

# Time-series plot:
plot(steps_by_interval$av_steps, type = "l")
# or, with actual intervals on x-axis:
plot(steps_by_interval$av_steps, x = steps_by_interval$interval, type = "l")

# Interval with the highest number of steps on average?
which.max(steps_by_interval$av_steps)
# Result: 104 (row index). This matches the info from the plot...

# Which interval is it exactly?
steps_by_interval[104,]
# or in one go:
steps_by_interval[which.max(steps_by_interval$av_steps),]
# Result: 835, 206.1698 steps on average...
# It seems like the subject is exercising in the morning (between 8:00 and 9:00)!


##### QUESTION #3######

# Imputing missing values...

# Total number of NA's in the dataset(s)?
summary(activity)
2304 / nrow(activity)

# Result:
# 2304 NA's
# or about 13.1 %

# Where are the NA's situated?
# Dataframe containing only the NA's:
na_activity <- activity %>% filter(is.na(steps) == TRUE)
nrow(na_activity)
# Also 2304...

# How many NA's per day?
na_by_day <- na_activity %>%
  # First group activity by date...
  group_by(date) %>%
  # Then summarise by summing number of rows with n()...
  summarise(na_count = n())

# Result: there seem to be 8 days with NA's. Judging by the number
# of NA's for each day (8 times 288) we can conclude that whole days
# are missing (all 288 intervals (12 x 24))...

# Which days of the week are missing?
na_by_day$weekday <- weekdays(na_by_day$date)
# For summary() to work, coerce weekday to a factor variable:
na_by_day$weekday <- as.factor(na_by_day$weekday)
n_distinct(na_by_day$weekday)
summary(na_by_day)

# Result:
#
# date               na_count        weekday 
# Min.   :2012-10-01   Min.   :288   Friday   :2  
# 1st Qu.:2012-10-26   1st Qu.:288   Monday   :2  
# Median :2012-11-06   Median :288   Saturday :1  
# Mean   :2012-11-01   Mean   :288   Sunday   :1  
# 3rd Qu.:2012-11-11   3rd Qu.:288   Thursday :1  
# Max.   :2012-11-30   Max.   :288   Wednesday:1
#
# So, the missing data seems to be spread pretty evenly over the weekdays (only
# Tuesday has no missing data; n-distinct() returned 6). Imputing by calculating
# typical averages for each day of the week? This seems reasonable, since data
# for _complete_ days seems to be missing.

# Actually imputing data into original dataset can be done by getting the average
# number of steps by day (by grouping and summarizing) (1). Subsequently joining
# the dataframe with the averages by day with the ungrouped dataframe (by day) makes
# the averages available. The NA values can now be easily replaced by them.

# Grouping by interval is also possible and works in similar fashion. We could try
# both so see what approach works best.

# Copy activity to activity_imputed:
activity_imputed <- activity
# Create column 'steps.imp' (set to '0' for clarity):
activity_imputed$steps.imp <- 0
# Add column 'weekday' (generated from 'date'):
activity_imputed$weekday <- as.factor(weekdays(activity_imputed$date))

# Create separate dataframe 'activity_weekday':
activity_weekday <- activity %>%
  mutate(weekday = as.factor(weekdays(date))) %>%
  group_by(weekday) %>%
  summarise(daily_steps = mean(steps, na.rm = TRUE))

# Now join the dataframes 'activity_imputed' and
# 'activity_weekday'on column 'weekday' (which is in both):
activity_imputed <- inner_join(activity_imputed, activity_weekday, by = "weekday")

# Replace value in 'steps.imp' with value in 'daily_steps' if
# 'steps' is NA...
activity_imputed$steps.imp[is.na(activity_imputed$steps)] <-
  activity_imputed$daily_steps[is.na(activity_imputed$steps)]

# Checking the result can be done by sampling the dataframe:
activity_imputed[sample(nrow(activity_imputed), 10), ]
# Rows with steps equaling NA should show 'daily_steps' as
# value in column 'steps.imp'. Other rows should show 0.
# Everything seems in order...

# Replace value in 'steps.imp' with value in 'steps' if
# 'steps' is NOT equal to NA...
activity_imputed$steps.imp[!is.na(activity_imputed$steps)] <-
  activity_imputed$steps[!is.na(activity_imputed$steps)]

# Make variable 'steps.imp' a numeric again (if neccesary):
activity_imputed$steps.imp <- as.numeric(activity_imputed$steps.imp)

# Clean the dataframe by removing 'daily_steps' column,
# since it's no longer relevant:
activity_imputed$daily_steps <- NULL

# Final check:
summary(activity_imputed)
# No NA's in the 'steps.imp' column!
# NB: values in 'steps.imp' are now fractions and should be
# rounded to the nearest integer before comparing with
# results from non-imputed data.

# Histogram of steps taken per day?
###################################

# What is total number of steps taken per day?
steps_by_day.imp <- activity_imputed %>%
  # First group activity by date...
  group_by(date) %>%
  # Then summarise by summing steps...
  summarise (daily_steps = sum(steps.imp))

# Histogram of total steps taken per day:
hist(round(steps_by_day.imp$daily_steps, 0))
# So, on most days the number of steps still lies between 10.000 and 15.000.
# There's hardly any visible difference when compared to the original
# (non-imputed) data.

# An alternative way of visualizing that is by means of a boxplot:
boxplot(steps_by_day.imp$daily_steps)


# What is mean total number of steps taken per day?
mean_total_steps.imp <- activity_imputed %>%
  # First group activity by date...
  group_by(date) %>%
  # Then summarise by summing steps...
  summarise (daily_steps = sum(steps.imp)) %>%
  # Summarise again by averaging daily steps...
  # Use na.rm = TRUE, otherwise the result is NA...
  summarise(mean_total_steps = median(daily_steps, na.rm = TRUE))

# Result:
mean_total_steps.imp
# ...11015 steps on average per day (the non-imputed data showed
# a slightly different result: 10765.
# That's a (neglectable?) difference of about 2.3%...(250 steps per day)


# What is median total number of steps taken per day?
median_total_steps.imp <- activity_imputed %>%
  # First group activity by date...
  group_by(date) %>%
  # Then summarise by summing steps...
  summarise (daily_steps = sum(steps.imp)) %>%
  # Summarise again by averaging daily steps...
  # Use na.rm = TRUE, otherwise the result is NA...
  summarise(median_total_steps = median(daily_steps, na.rm = TRUE))

# Result:
median_total_steps.imp
# ...11015 steps on average per day.
# Compared to 10765 on average for the non-imputed data.
# A (neglectable?) difference of 250 steps per day.



##### QUESTION #4 ######

# Are there differences in activity patterns between weekdays and weekends?

# Using 'activity_imputed'...
# Adding 'day_type' (factor)variable.
# First setting 'day_type' to "Weekend":
activity_imputed$day_type[activity_imputed$weekday == "Saturday" |
                            activity_imputed$weekday == "Sunday"] <- "Weekend"
# Now setting the complement to "Weekday":
activity_imputed$day_type[!(activity_imputed$weekday == "Saturday" |
                            activity_imputed$weekday == "Sunday")] <- "Weekday"
# Coercing 'day_type' to factor variable:
activity_imputed$day_type <- as.factor(activity_imputed$day_type)

# Checking the result by sampling the data:
activity_imputed[sample(nrow(activity_imputed), 10), ]
# Everything seems to be in order — only Saturdays / Sundays are
# of type "Weekend".


# What is the average daily activity pattern during the weekends?
steps_by_interval_weekend <- activity_imputed %>%
  # Filter by 'day_type":
  filter(day_type == "Weekend") %>%
  # First group activity by date...
  group_by(interval) %>%
  # Then summarise by summing steps...
  summarise (av_steps = mean(steps.imp, na.rm = TRUE))

# Result:
steps_by_interval_weekend

# Time-series plot:
plot(round(steps_by_interval_weekend$av_steps, 0), type = "l")
# or, with actual intervals on x-axis:
plot(round(steps_by_interval_weekend$av_steps, 0), x = steps_by_interval$interval, type = "l")



# What is the average daily activity pattern during weekdays?
steps_by_interval_weekdays <- activity_imputed %>%
  # Filter by 'day_type":
  filter(day_type == "Weekday") %>%
  # First group activity by interval...
  group_by(interval) %>%
  # Then summarise by summing steps...
  summarise (av_steps = mean(steps.imp, na.rm = TRUE))

# Result:
steps_by_interval_weekdays

# Time-series plot:
plot(round(steps_by_interval_weekdays$av_steps, 0), type = "l")
# or, with actual intervals on x-axis:
plot(round(steps_by_interval_weekdays$av_steps, 0), x = steps_by_interval$interval, type = "l")

# Creating 'steps_by_interval.imp', since we need that for the panel plot:
steps_by_interval.imp <- activity_imputed %>%
  # First group activity by interval...
  # Keep day_type (needed for plotting).
  group_by(interval, day_type) %>%
  # Then summarise by summing steps...
  summarise (av_steps = round(mean(steps.imp, na.rm = TRUE), 0))


# Panelplot? Showing weekend and weekdays side by side...
# Judging by the look of the example plot lattice should be used here.
install.packages("lattice")
library(lattice)

# This code produces the required plot:
xyplot(av_steps ~ interval | day_type, data = steps_by_interval.imp,
       type = "l", layout = c(1,2),
       ylab = "Number of steps",
       xlab = "Interval",
       main = "Activity Pattern")