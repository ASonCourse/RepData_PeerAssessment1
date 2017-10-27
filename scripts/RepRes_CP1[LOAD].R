# This file / script describes the steps involved in loading the data into R. The
# code is minimally commented, since the .Rmd file contains a thorough description.

# First, let's fix the date we run this script / download the data:
Download_Date <- Sys.Date()

# Next let's download the data from the internet:
source_url <- "https://raw.githubusercontent.com/ASonCourse/RepData_PeerAssessment1/master/activity.zip"

download.file(source_url, destfile = "activity.zip", 
              method = "curl")

# Unzip downloaded file (unzipped file -> "activity.csv"):
unzip("activity.zip", exdir = ".")

# Importing .csv file in R:
activity <- read.csv("activity.csv", stringsAsFactors = FALSE)

# Confirm the data was imported properly:
dim(activity)
# There should be 17.568 observations — and there are exactly that many.
# So the import is complete.
str(activity)
min(activity$date)
max(activity$date)
summary(activity)

# How many NA's?
mean(is.na(activity$steps))

