# This file / script describes the steps involved in cleaning the imported data. The
# code is minimally commented, since the .Rmd file contains a thorough description.

# Transforming the date variable from character to date class:
activity$date <- as.Date(activity$date, format = "%Y-%m-%d")

