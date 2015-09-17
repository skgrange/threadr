# **threadr**

**threadr** is a collection of R utility functions to link pieces of analysis together.

## To install:

The development version: `devtools::install_github("skgrange/threadr")`

## To-do: 

  1. Do some unit testing
  2. Work on documentation
  3. Get package on CRAN

## Some examples

### Padding time-series

When dealing with time-series data, often one of the most important things to do is to ensure that the time-series is uniform, *i.e.* all dates which occurred during the period of observation are present. `time_pad` is a robust function which does exactly that. There are also helpful extensions such as starting the time-series at the beginning of a hour/day/month/year, and ensuring that identifying variables are added to the data after time-padding has occurred.

```
# Set-up
library(threadr)

# Load data
data.air <- read.csv("oxford_road_air_quality_data.csv")

# Parse dates
data.air$date <- ymd_hms(data.air$date)

# Pad time-series
data.air.pad <- time_pad(
  data.air, interval = "hour", by = c("site", "site.name"), round = "day")
```

### Round dates to arbitrary time intervals

Dealing with multiple data sources which have observations at different time intervals can be frustrating. More frustration can occur when the different data sources begin at an unhelpful times such as `2015-07-10 09:21:42`. `round_time_interval` allows issues likes these to be resolved to allow for future joining or aggregation of values from different sources.

```
# Set-up
library(threadr)

# Load data
data.gps <- read.csv("gps_track_data.csv")
data.sensor <- read.csv("co2_sensor_data.csv")

# Parse dates
data.gps$date <- ymd_hms(data.gps$date)
data.sensor$date <- ymd_hms(data.sensor$date)

# Round both data sources to 5-second intervals
data.gps.clean$date <- round_date_interval(data.gps.clean$date, "5 sec")
data.sensor.clean$date <- round_date_interval(data.gps.sensor$date, "5 sec")

# Join data
data.join <- merge(data.gps.clean, data.sensor.clean, by = "date", all = TRUE)
```

### Export data frame to JSON

`write.json` is a simple wrapper for `jsonlite::toJSON` which allows for quick JSON export in the same way as `write.csv`.

