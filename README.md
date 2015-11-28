# **threadr**

**threadr** is a collection of R utility functions to link pieces of analysis together. **threadr** is not a focused, single-unit package, rather a collection of functions which I use to glue many processes together.

## To install:

The development version: `devtools::install_github("skgrange/threadr")`

## To-do: 

  1. Do some unit testing
  2. Work on documentation
  3. Get package on CRAN
  
## A glance

  - Utilities:
    - `write_json` is a simple wrapper for `jsonlite::toJSON` which allows for quick JSON export in the same way as `write.csv`.
    - `download_ftp_file` and `upload_to_ftp`. 
    - `str_*` functions. Do things which **stringr** does not. 
      - `str_proper_case`, `str_trim_length`, `str_sentence_case`, `str_trim_many_spaces`, `str_underscore`, `str_chop`
    - Write JSON files with `write_json`. 
    - A number of unit conversion functions.
      - Simple ones like: `miles_to_km`, `knots_to_km_h`, `kw_to_hp`, `fahrenheit_to_celsius`, and `psi_to_bar`. 
      - More interesting ones like `mpg_to_l_100_km`, `mpg_to_km_l` (for fuel efficiency and consumption). 
      
  - Date functions
    - Pad time-series to different intervals with `time_pad`. 
    - Round dates to arbitrary time intervals such as 5-seconds, 5-minutes, 15-minutes, 2-minutes, 30-minutes (etc.) with `round_date_interval`
    
  - Data frame functions: 
    - `add_row_numbers`. Very similar to `dplyr::add_rownames` but the variable is an integer, not a character. 
    - `arrange_left`. Move variables/columns to the left of a data frame. 
    - `base_df`. Remove **dplyr**'s `data.frame` extension (`tbl_df` and others) from a data frame. I have encountered issues with some functions when piping data frames within the **dplyr**'s grammar; especially those interacting with SQL databases. 
    - `drop_na_columns`
    - `rm_na_rows`
    
  - Database functions:
    - `db_connect` 
    - `db_insert`
    - `db_contents` 
    - `db_count_rows`

## Some examples

### Padding time-series

When dealing with time-series data, often one of the most important things to do is to ensure that the time-series is uniform, *i.e.* all dates which occurred during the period of observation are present. `time_pad` is a robust function which does exactly that. There are also helpful extensions such as starting the time-series at the beginning of a hour/day/month/year, and ensuring that identifying variables are added to the data after time-padding has occurred.

```
# Set-up
library(threadr)

# Load data
data_air <- read.csv("oxford_road_air_quality_data.csv")

# Parse dates
data_air$date <- ymd_hms(data_air$date)

# Pad time-series
data_air_pad <- time_pad(
  data_air, interval = "hour", by = c("site", "site_name"), round = "day")
```

### Round dates to arbitrary time intervals

Dealing with multiple data sources which have observations at different time intervals can be frustrating. More frustration can occur when the different data sources begin at an unhelpful times such as `2015-07-10 09:21:42`. `round_time_interval` allows issues likes these to be resolved to allow for future joining or aggregation of values from different sources.

```
# Set-up
library(threadr)

# Load data
data_gps <- read.csv("gps_track_data.csv")
data_sensor <- read.csv("co2_sensor_data.csv")

# Parse dates
data_gps$date <- ymd_hms(data_gps$date)
data_sensor$date <- ymd_hms(data_sensor$date)

# Round both data sources to 5-second intervals
data_gps_clean$date <- round_date_interval(data_gps_clean$date, "5 sec")
data_sensor_clean$date <- round_date_interval(data_gps_sensor$date, "5 sec")

# Join data
data_join <- merge(data_gps_clean, data_sensor_clean, by = "date", all = TRUE)
```
