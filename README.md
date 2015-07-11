# **threadr**

**threadr** is a collection of R utility functions to link pieces of analysis together.

## To install:

The development version: `devtools::install_github('skgrange/threadr')`

## To-do: 

  1. Do some unit testing
  2. Work on documentation
  3. Get package on CRAN

## Some explanation

### Padding time-series

When dealing with time-series data, one of the most important things to do is to ensure that the time-series is uniform, *i.e.* all dates which occurred during the period of observation are present. `time_pad` is a robust function which does exactly that with helpful extensions such as starting the time-series at the beginning of a hour/day/month/year and ensuring that identifying variables are added to the data after time-padding has occurred. 

### Round messy dates to arbitrary time intervals

Dealing with multiple data sources which have observations at different time intervals can be frustrating. More frustration can occur when the different data sources begin at an unhelpful times such as `2015-07-10 09:21:42`. `round_time_interval` allows issues likes these to be resolved to allow for future joining or aggregation of values from different sources.

### `unix_time`

Dealing with dates and time-zones can get confusing quickly. Unix time is an numeric value which is time-zone independent and therefore has no ambiguity. Although Unix time is opaque for humans, it can be very useful to store alongside dates. `unix_time` allows for this date to Unix time transformation to occur easily. 

### `write.json`

`write.json` is a simple wrapper for `jsonlite::toJSON` which allows for very quick JSON export in the same way as `write.table`/`write.csv`. 

### `write.gpx`

Like `write.json`, `write.gpx` is a wrapper for a function which writes GPX files. However there is a bit more complexity due to the limitations of the wrapped function and the different types of spatial data GPX files can contain. Again, the usage is very similar to `write.csv`. 
